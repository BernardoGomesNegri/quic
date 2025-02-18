{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QUIC.Server.Reader (
    Dispatch(..)
  , newDispatch
  , clearDispatch
  , runDispatcher
  -- * Accepting
  , accept
  , Accept(..)
  -- * Receiving and reading
  , RecvQ
  , recvServer
  , readerServer
  -- * Misc
  , udpServerClientSocket
  , SockDict(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Crypto.Token as CT
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified GHC.IO.Exception as E
import Network.ByteOrder
import Network.Socket hiding (accept, Debug)
import qualified Network.Socket.ByteString as NSB
import qualified System.IO.Error as E
import qualified UnliftIO.Exception as E
import System.IO

import Network.QUIC.Config
import Network.QUIC.Connection
import Network.QUIC.Exception
import Network.QUIC.Imports
import Network.QUIC.Logger
import Network.QUIC.Packet
import Network.QUIC.Parameters
import Network.QUIC.Socket
import Network.QUIC.Types

----------------------------------------------------------------

data Dispatch = Dispatch {
    tokenMgr :: CT.TokenManager
  , dstTable :: IORef ConnectionDict
  , srcTable :: IORef RecvQDict
  , sockTable :: IORef SockDict
  , acceptQ  :: AcceptQ
  }

newDispatch :: IO Dispatch
newDispatch = Dispatch <$> CT.spawnTokenManager CT.defaultConfig
                       <*> newIORef emptyConnectionDict
                       <*> newIORef emptyRecvQDict
                       <*> newIORef (SockDict M.empty)
                       <*> newAcceptQ

clearDispatch :: Dispatch -> IO ()
clearDispatch d = CT.killTokenManager $ tokenMgr d

----------------------------------------------------------------

newtype ConnectionDict = ConnectionDict (Map CID Connection)

newtype SockDict = SockDict (Map SockAddr ToClientSocket)

emptyConnectionDict :: ConnectionDict
emptyConnectionDict = ConnectionDict M.empty

lookupConnectionDict :: IORef ConnectionDict -> CID -> IO (Maybe Connection)
lookupConnectionDict ref cid = do
    ConnectionDict tbl <- readIORef ref
    return $ M.lookup cid tbl

registerConnectionDict :: IORef ConnectionDict -> CID -> Connection -> IO ()
registerConnectionDict ref cid conn = atomicModifyIORef'' ref $
    \(ConnectionDict tbl) -> ConnectionDict $ M.insert cid conn tbl

unregisterConnectionDict :: IORef ConnectionDict -> CID -> IO ()
unregisterConnectionDict ref cid = atomicModifyIORef'' ref $
    \(ConnectionDict tbl) -> ConnectionDict $ M.delete cid tbl

----------------------------------------------------------------

-- Original destination CID -> RecvQ
data RecvQDict = RecvQDict Int (OrdPSQ CID Int RecvQ)

recvQDictSize :: Int
recvQDictSize = 100

emptyRecvQDict :: RecvQDict
emptyRecvQDict = RecvQDict 0 PSQ.empty

lookupRecvQDict :: IORef RecvQDict -> CID -> IO (Maybe RecvQ)
lookupRecvQDict ref dcid = do
    RecvQDict _ qt <- readIORef ref
    return $ case PSQ.lookup dcid qt of
      Nothing     -> Nothing
      Just (_,q)  -> Just q

insertRecvQDict :: IORef RecvQDict -> CID -> RecvQ -> IO ()
insertRecvQDict ref dcid q = atomicModifyIORef'' ref ins
  where
    ins (RecvQDict p qt0) = let qt1 | PSQ.size qt0 <= recvQDictSize = qt0
                                    | otherwise = PSQ.deleteMin qt0
                                qt2 = PSQ.insert dcid p q qt1
                                p' = p + 1 -- fixme: overflow
                            in RecvQDict p' qt2

----------------------------------------------------------------

data Accept = Accept {
    accVersionInfo  :: VersionInfo
  , accMyAuthCIDs   :: AuthCIDs
  , accPeerAuthCIDs :: AuthCIDs
  , accMySockAddr   :: SockAddr
  , accPeerSockAddr :: SockAddr
  , accRecvQ        :: RecvQ
  , accPacketSize   :: Int
  , accRegister     :: CID -> Connection -> IO ()
  , accUnregister   :: CID -> IO ()
  , accAddressValidated :: Bool
  , accTime         :: TimeMicrosecond
  , accSock :: Socket
  }

newtype AcceptQ = AcceptQ (TQueue Accept)

newAcceptQ :: IO AcceptQ
newAcceptQ = AcceptQ <$> newTQueueIO

readAcceptQ :: AcceptQ -> IO Accept
readAcceptQ (AcceptQ q) = atomically $ readTQueue q

writeAcceptQ :: AcceptQ -> Accept -> IO ()
writeAcceptQ (AcceptQ q) x = atomically $ writeTQueue q x

accept :: Dispatch -> IO Accept
accept = readAcceptQ . acceptQ

----------------------------------------------------------------

runDispatcher :: Dispatch -> ServerConfig -> (Socket, SockAddr) -> IO ThreadId
runDispatcher d conf ssa@(s,_) =
    forkFinally (dispatcher d conf ssa) $ \_ -> close s

dispatcher :: Dispatch -> ServerConfig -> (Socket, SockAddr) -> IO ()
dispatcher d@Dispatch{sockTable = sockDict} conf (s,mysa) = handleLogUnit logAction body
  where
    body = do
    --    let (opt,_cmsgid) = case mysa of
    --          SockAddrInet{}  -> (RecvIPv4PktInfo, CmsgIdIPv4PktInfo)
    --          SockAddrInet6{} -> (RecvIPv6PktInfo, CmsgIdIPv6PktInfo)
    --          _               -> error "dispatcher"
    --    setSocketOption s opt 1
        forever $ do
    --        (peersa, bs0, _cmsgs, _) <- recv
            (bs0, peersa) <- recv
            putStrLn "server: got stuff from client"
            (SockDict sockDict') <- atomicModifyIORef sockDict (\x -> (x,x))
            case M.lookup peersa sockDict' of
                Just (ToClientSocket entryQ _ _) -> do
                    putStrLn "server: matched stuff with client"
                    atomically $ writeTQueue entryQ bs0
                Nothing -> do
                    putStrLn "server: new peer that is not on dictionary"
                    let bytes = BS.length bs0 -- both Initial and 0RTT
                    now <- getTimeMicrosecond
                    print (bytes, now) >> hFlush stdout
                    -- macOS overrides the local address of the socket
                    -- if in_pktinfo is used.
                    (pkt, bs0RTT) <- decodePacket bs0
            --        let send bs = void $ NSB.sendMsg s peersa [bs] cmsgs' 0
                    let send bs = void $ NSB.sendTo s bs peersa
                    dispatch d conf logAction pkt mysa peersa send bs0RTT bytes now s
    doDebug = isJust $ scDebugLog conf
    logAction msg | doDebug   = stdoutLogger ("dispatch(er): " <> msg)
                  | otherwise = return ()
    recv = do
--        ex <- E.try $ NSB.recvMsg s maximumUdpPayloadSize 64 0
        ex <- E.tryAny $ NSB.recvFrom s maximumUdpPayloadSize
        case ex of
           Right x -> return x
           Left se -> case E.fromException se of
              Just e | E.ioeGetErrorType e == E.InvalidArgument -> E.throwIO se
              _ -> do
                  logAction $ "recv again: " <> bhow se
                  recv

----------------------------------------------------------------

-- If client initial is fragmented into multiple packets,
-- there is no way to put the all packets into a single queue.
-- Rather, each fragment packet is put into its own queue.
-- For the first fragment, handshake would successif others are
-- retransmitted.
-- For the other fragments, handshake will fail since its socket
-- cannot be connected.
dispatch :: Dispatch -> ServerConfig -> DebugLogger -> PacketI -> SockAddr -> SockAddr -> (ByteString -> IO ()) -> ByteString -> Int -> TimeMicrosecond -> Socket -> IO ()
dispatch Dispatch{..} ServerConfig{..} logAction
         (PacketIC cpkt@(CryptPacket (Initial peerVer dCID sCID token) _) lvl)
         mysa peersa send bs0RTT bytes tim sock
  | bytes < defaultQUICPacketSize = do
        logAction $ "too small " <> bhow bytes <> ", " <> bhow peersa
  | peerVer `notElem` myVersions = do
        let offerVersions
                | peerVer == GreasingVersion = GreasingVersion2 : myVersions
                | otherwise                  = GreasingVersion  : myVersions
        bss <- encodeVersionNegotiationPacket $ VersionNegotiationPacket sCID dCID offerVersions
        send bss
  | token == "" = do
        mq <- lookupConnectionDict dstTable dCID
        case mq of
          Nothing
            | scRequireRetry -> sendRetry
            | otherwise      -> pushToAcceptFirst False
          _                  -> return ()
  | otherwise = do
        mct <- decryptToken tokenMgr token
        case mct of
          Just ct
            | isRetryToken ct -> do
                  ok <- isRetryTokenValid ct
                  if ok then pushToAcceptRetried ct else sendRetry
            | otherwise -> do
                  mq <- lookupConnectionDict dstTable dCID
                  case mq of
                    Nothing -> pushToAcceptFirst True
                    _       -> return ()
          _ -> sendRetry
  where
    myVersions = otherVersions scVersionInfo
    pushToAcceptQ myAuthCIDs peerAuthCIDs key addrValid = do
        mq <- lookupRecvQDict srcTable key
        case mq of
          Just q -> writeRecvQ q $ mkReceivedPacket cpkt tim bytes lvl
          Nothing -> do
              q <- newRecvQ
              insertRecvQDict srcTable key q
              writeRecvQ q $ mkReceivedPacket cpkt tim bytes lvl
              let reg = registerConnectionDict dstTable
                  unreg = unregisterConnectionDict dstTable
                  ent = Accept {
                      accVersionInfo  = VersionInfo peerVer myVersions
                    , accMyAuthCIDs   = myAuthCIDs
                    , accPeerAuthCIDs = peerAuthCIDs
                    , accMySockAddr   = mysa
                    , accPeerSockAddr = peersa
                    , accRecvQ        = q
                    , accPacketSize   = bytes
                    , accRegister     = reg
                    , accUnregister   = unreg
                    , accAddressValidated = addrValid
                    , accTime         = tim
                    , accSock         = sock
                    }
              -- fixme: check acceptQ length
              writeAcceptQ acceptQ ent
              when (bs0RTT /= "") $ do
                  (PacketIC cpktRTT0 lvl', _) <- decodePacket bs0RTT
                  writeRecvQ q $ mkReceivedPacket cpktRTT0 tim bytes lvl'
    -- Initial: DCID=S1, SCID=C1 ->
    --                                     <- Initial: DCID=C1, SCID=S2
    --                               ...
    -- 1-RTT: DCID=S2 ->
    --                                                <- 1-RTT: DCID=C1
    --
    -- initial_source_connection_id       = S2   (newdCID)
    -- original_destination_connection_id = S1   (dCID)
    -- retry_source_connection_id         = Nothing
    pushToAcceptFirst addrValid = do
        newdCID <- newCID
        let myAuthCIDs = defaultAuthCIDs {
                initSrcCID  = Just newdCID
              , origDstCID  = Just dCID
              }
            peerAuthCIDs = defaultAuthCIDs {
                initSrcCID = Just sCID
              }
        pushToAcceptQ myAuthCIDs peerAuthCIDs dCID addrValid
    -- Initial: DCID=S1, SCID=C1 ->
    --                                       <- Retry: DCID=C1, SCID=S2
    -- Initial: DCID=S2, SCID=C1 ->
    --                                     <- Initial: DCID=C1, SCID=S3
    --                               ...
    -- 1-RTT: DCID=S3 ->
    --                                                <- 1-RTT: DCID=C1
    --
    -- initial_source_connection_id       = S3   (dCID)  S2 in our server
    -- original_destination_connection_id = S1   (o)
    -- retry_source_connection_id         = S2   (dCID)
    pushToAcceptRetried (CryptoToken _ _ (Just (_,_,o))) = do
        let myAuthCIDs = defaultAuthCIDs {
                initSrcCID  = Just dCID
              , origDstCID  = Just o
              , retrySrcCID = Just dCID
              }
            peerAuthCIDs = defaultAuthCIDs {
                initSrcCID = Just sCID
              }
        pushToAcceptQ myAuthCIDs peerAuthCIDs o True
    pushToAcceptRetried _ = return ()
    isRetryTokenValid (CryptoToken tver etim (Just (l,r,_))) = do
        diff <- getElapsedTimeMicrosecond etim
        return $ tver == peerVer
              && diff <= Microseconds 30000000 -- fixme
              && dCID == l
              && sCID == r
    isRetryTokenValid _ = return False
    sendRetry = do
        newdCID <- newCID
        retryToken <- generateRetryToken peerVer newdCID sCID dCID
        mnewtoken <- timeout (Microseconds 100000) $ encryptToken tokenMgr retryToken
        case mnewtoken of
          Nothing       -> logAction "retry token stacked"
          Just newtoken -> do
              bss <- encodeRetryPacket $ RetryPacket peerVer sCID newdCID newtoken (Left dCID)
              send bss
dispatch Dispatch{..} _ _
         (PacketIC cpkt@(CryptPacket (RTT0 _ o _) _) lvl) _ _peersa _ _ bytes tim _ = do
    mq <- lookupRecvQDict srcTable o
    case mq of
      Just q  -> writeRecvQ q $ mkReceivedPacket cpkt tim bytes lvl
      Nothing -> return ()
dispatch Dispatch{..} _ logAction
         (PacketIC cpkt@(CryptPacket hdr _crypt) lvl) _mysa peersa _ _ bytes tim _ = do
    -- fixme: packets for closed connections also match here.
    let dCID = headerMyCID hdr
    mx <- lookupConnectionDict dstTable dCID
    case mx of
      Nothing -> do
          putStrLn "Nothing" >> hFlush stdout
          logAction $ "CID no match: " <> bhow dCID <> ", " <> bhow peersa
      Just conn -> do
          putStrLn "Just" >> hFlush stdout
          writeRecvQ (connRecvQ conn) $ mkReceivedPacket cpkt tim bytes lvl

dispatch _ _ _ _ipkt _ _peersa _ _ _ _ _ = return ()

----------------------------------------------------------------

udpServerClientSocket :: IORef SockDict -> SockAddr -> IO ToClientSocket
udpServerClientSocket table peersa = do
    (entryQ, exitQ) <- atomically ((,) <$> newTQueue <*> newTQueue)
    let toSock = ToClientSocket entryQ exitQ peersa
    atomicModifyIORef'' table $ \(SockDict d) -> (SockDict . M.insert peersa toSock) d
    return toSock

-- | readerServer dies when the socket is closed.
readerServer :: ToClientSocket -> Connection -> IORef SockDict -> IO ()
readerServer (ToClientSocket entryQ _ peer) conn sockDict = handleLogUnit logAction loop
  where
    loop = do
        (Microseconds ito) <- readMinIdleTimeout conn
        delayBox <- registerDelay ito
        mbs <- atomically $ do
            timeouted <- readTVar delayBox
            if timeouted then
                return Nothing
            else do
                item <- tryReadTQueue entryQ
                case item of
                    Nothing -> retry
                    Just x -> return (return x)
        case mbs of
          Nothing -> do 
              putStrLn "server: no data, closing socket"
              closeTC
          Just bs -> do
              now <- getTimeMicrosecond
              let bytes = BS.length bs
              addRxBytes conn bytes
              pkts <- decodeCryptPackets bs
              mapM_ (\(p,l) -> writeRecvQ (connRecvQ conn) (mkReceivedPacket p now bytes l)) pkts
              loop
    closeTC = atomicModifyIORef'' sockDict (\(SockDict x) -> SockDict $ M.delete peer x)
    logAction msg = connDebugLog conn ("debug: readerServer: " <> msg)

recvServer :: RecvQ -> IO ReceivedPacket
recvServer = readRecvQ
