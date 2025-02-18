{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.QUIC.Client.Reader (
    readerClient
  , runNewClientReader
  , recvClient
  , ConnectionControl(..)
  , controlConnection
  ) where

import Control.Concurrent
import qualified Data.ByteString as BS
import Data.List (intersect)
import Network.Socket (Socket, getSocketName, getPeerName, close, SockAddr)
import qualified Network.Socket.ByteString as NSB
import qualified UnliftIO.Exception as E

import Network.QUIC.Connection
import Network.QUIC.Connector
import Network.QUIC.Crypto
import Network.QUIC.Exception
import Network.QUIC.Imports
import Network.QUIC.Packet
import Network.QUIC.Parameters
import Network.QUIC.Qlog
import Network.QUIC.Recovery
import Network.QUIC.Socket
import Network.QUIC.Types
import Network.QUIC.Logger (bhow)
import System.Log.FastLogger (ToLogStr(toLogStr))

-- | readerClient dies when the socket is closed.
readerClient :: Socket -> Connection -> IO ()
readerClient s0 conn = handleLogUnit logAction $ do
    wait
    getServerAddr conn >>= loop
  where
    wait = do
        bound <- E.handleAny (\_ -> return False) $ do
            _ <- getSocketName s0
            return True
        unless bound $ do
            yield
            wait
    loop msa0 = do
        ito <- readMinIdleTimeout conn
        mbs <- timeout ito $ do
            case msa0 of
              Nothing  -> do
                --print "about to recv"
                x <- NSB.recv     s0 maximumUdpPayloadSize
                --print "got something"
                return x
              Just sa0 -> do
                  putStr "recvFrom getSocketName "
                  (getSocketName s0 >>= print) `E.catchAny` \e -> print e
                  putStr "recvFrom getPeerName "
                  (getPeerName s0 >>= print) `E.catchAny` \e -> print e
                  putStrLn "recvFrom..."
                  (bs, sa) <- NSB.recvFrom s0 maximumUdpPayloadSize
                  putStrLn "recvFrom...done"
                  return $ if sa == sa0 then bs else ""
        case mbs of
          Nothing -> close s0
          Just "" -> loop msa0
          Just bs -> do
            now <- getTimeMicrosecond
            let bytes = BS.length bs
            addRxBytes conn bytes
            pkts <- decodePackets bs
            mapM_ (putQ now bytes) pkts
            loop msa0
    logAction msg = connDebugLog conn ("debug: readerClient: " <> msg)
    putQ _ _ (PacketIB BrokenPacket) = return ()
    putQ t _ (PacketIV pkt@(VersionNegotiationPacket dCID sCID peerVers)) = do
        qlogReceived conn pkt t
        myVerInfo <- getVersionInfo conn
        let myVer   = chosenVersion myVerInfo
            myVers0 = otherVersions myVerInfo
        -- ignoring VN if the original version is included.
        when (myVer `notElem` peerVers && Negotiation `notElem` peerVers) $ do
            ok <- checkCIDs conn dCID (Left sCID)
            let myVers = filter (not . isGreasingVersion) myVers0
                nextVerInfo = case myVers `intersect` peerVers of
                  vers@(ver:_) | ok -> VersionInfo ver vers
                  _                 -> brokenVersionInfo
            E.throwTo (mainThreadId conn) $ VerNego nextVerInfo
    putQ t z (PacketIC pkt lvl) = writeRecvQ (connRecvQ conn) $ mkReceivedPacket pkt t z lvl
    putQ t _ (PacketIR pkt@(RetryPacket ver dCID sCID token ex)) = do
        qlogReceived conn pkt t
        ok <- checkCIDs conn dCID ex
        when ok $ do
            resetPeerCID conn sCID
            setPeerAuthCIDs conn $ \auth -> auth { retrySrcCID  = Just sCID }
            initializeCoder conn InitialLevel $ initialSecrets ver sCID
            setToken conn token
            setRetried conn True
            releaseByRetry (connLDCC conn) >>= mapM_ put
      where
        put ppkt = putOutput conn $ OutRetrans ppkt

checkCIDs :: Connection -> CID -> Either CID (ByteString,ByteString) -> IO Bool
checkCIDs conn dCID (Left sCID) = do
    localCID <- getMyCID conn
    remoteCID <- getPeerCID conn
    return (dCID == localCID && sCID == remoteCID)
checkCIDs conn dCID (Right (pseudo0,tag)) = do
    localCID <- getMyCID conn
    remoteCID <- getPeerCID conn
    ver <- getVersion conn
    let ok = calculateIntegrityTag ver remoteCID pseudo0 == tag
    return (dCID == localCID && ok)

recvClient :: RecvQ -> IO ReceivedPacket
recvClient = readRecvQ

----------------------------------------------------------------

-- | How to control a connection.
data ConnectionControl = ChangeServerCID
                       | ChangeClientCID
                       | NATRebinding
                       | ActiveMigration
                       deriving (Eq, Show)

controlConnection :: Connection -> ConnectionControl -> IO Bool
controlConnection conn typ
  | isClient conn = do
        waitEstablished conn
        controlConnection' conn typ
  | otherwise     = return False

controlConnection' :: Connection -> ConnectionControl -> IO Bool
controlConnection' conn ChangeServerCID = do
    mn <- timeout (Microseconds 1000000) $ waitPeerCID conn -- fixme
    case mn of
      Nothing              -> return False
      Just (CIDInfo n _ _) -> do
          sendFrames conn RTT1Level [RetireConnectionID n]
          return True
controlConnection' conn ChangeClientCID = do
    cidInfo <- getNewMyCID conn
    x <- (+1) <$> getMyCIDSeqNum conn
    sendFrames conn RTT1Level [NewConnectionID cidInfo x]
    return True
controlConnection' conn NATRebinding = do
    rebind conn $ Microseconds 5000 -- nearly 0
    return True
controlConnection' conn ActiveMigration = do
    mn <- timeout (Microseconds 1000000) $ waitPeerCID conn -- fixme
    case mn of
      Nothing  -> return False
      mcidinfo -> do
          rebind conn $ Microseconds 5000000
          validatePath conn mcidinfo
          return True

rebind :: Connection -> Microseconds -> IO ()
rebind conn microseconds = do
    s0:_ <- getSockets conn
    msa0 <- getServerAddr conn
    s1 <- case msa0 of
      Nothing  -> getPeerName s0 >>= udpNATRebindingConnectedSocket
      Just sa0 -> udpNATRebindingSocket sa0
    _ <- addSocket conn s1
    let reader = readerClient s1 conn
    forkIO reader >>= addReader conn
    fire conn microseconds $ close s0

-- Copy-paste of old function because the client depends on this
readerClient' :: Socket -> Connection -> IO ()
readerClient' s conn = handleLogUnit logAction loop
  where
    loop = do
        ito <- readMinIdleTimeout conn
        mbs <- timeout ito $ NSB.recv s maximumUdpPayloadSize
        case mbs of
          Nothing -> close s
          Just bs -> do
              now <- getTimeMicrosecond
              let bytes = BS.length bs
              addRxBytes conn bytes
              pkts <- decodeCryptPackets bs
              mapM_ (\(p,l) -> writeRecvQ (connRecvQ conn) (mkReceivedPacket p now bytes l)) pkts
              loop
    logAction msg = connDebugLog conn ("debug: readerServer: " <> msg)

-- Copy-paste of old function because the client depends on this
runNewClientReader :: Connection -> SockAddr -> SockAddr -> CID -> IO ()
runNewClientReader conn mysa peersa dCID = handleLogUnit logAction $ do
    migrating <- isPathValidating conn -- fixme: test and set
    unless migrating $ do
        setMigrationStarted conn
        -- fixme: should not block
        mcidinfo <- timeout (Microseconds 100000) $ waitPeerCID conn
        let msg = "Migration: " <> bhow peersa <> " (" <> bhow dCID <> ")"
        qlogDebug conn $ Debug $ toLogStr msg
        connDebugLog conn $ "debug: runNewServerReader: " <> msg
        E.bracketOnError setup close $ \s1 ->
            E.bracket (addSocket conn s1) close $ \_ -> do
                void $ forkIO $ readerClient' s1 conn
                -- fixme: if cannot set
                setMyCID conn dCID
                validatePath conn mcidinfo
                -- holding the old socket for a while
                delay $ Microseconds 20000
  where
    setup = udpClientConnectedSocket' mysa peersa
    logAction msg = connDebugLog conn ("debug: runNewServerReader: " <> msg)