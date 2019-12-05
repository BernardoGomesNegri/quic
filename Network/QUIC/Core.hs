{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QUIC.Core where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Network.TLS.QUIC
import System.Timeout
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Network.QUIC.Config
import Network.QUIC.Connection
import Network.QUIC.Handshake
import Network.QUIC.Imports
import Network.QUIC.Packet
import Network.QUIC.Receiver
import Network.QUIC.Route
import Network.QUIC.Sender
import Network.QUIC.Socket
import Network.QUIC.Types

----------------------------------------------------------------

data QUICClient = QUICClient {
    clientConfig :: ClientConfig
  }

data QUICServer = QUICServer {
    serverConfig :: ServerConfig
  , serverRoute  :: ServerRoute
  }

----------------------------------------------------------------

withQUICClient :: ClientConfig -> (QUICClient -> IO ()) -> IO ()
withQUICClient conf body = do
    let qc = QUICClient conf
    body qc

connect :: QUICClient -> IO Connection
connect QUICClient{..} = E.handle tlserr $ do
    s <- udpClientConnectedSocket (ccServerName clientConfig) (ccPortName clientConfig)
    let send bss = void $ NSB.sendMany s bss
        recv     = NSB.recv s 2048 >>= decodeCryptPackets
    myCID   <- newCID
    peerCID <- newCID
    conn <- clientConnection clientConfig myCID peerCID send recv
    tid0 <- forkIO $ sender conn
    tid1 <- forkIO $ receiver conn
    tid2 <- forkIO $ resender conn
    setThreadIds conn [tid0,tid1,tid2]
    handshakeClient clientConfig conn
    setConnectionStatus conn Open
    return conn
  where
    tlserr e = E.throwIO $ HandshakeFailed $ show $ errorToAlertDescription e

----------------------------------------------------------------

withQUICServer :: ServerConfig -> (QUICServer -> IO ()) -> IO ()
withQUICServer conf body = do
    route <- newServerRoute
    ssas <- mapM  udpServerListenSocket $ scAddresses conf
    tids <- mapM (runRouter route) ssas
    let qs = QUICServer conf route
    body qs `E.finally` mapM_ killThread tids
  where
    runRouter route ssa@(s,_) = forkFinally (router conf route ssa) (\_ -> NS.close s)

accept :: QUICServer -> IO Connection
accept QUICServer{..} = E.handle tlserr $ do
    Accept myCID peerCID oCID mysa peersa q <- atomically $ readTQueue $ acceptQueue serverRoute
    s <- udpServerConnectedSocket mysa peersa
    let send bss = void $ NSB.sendMany s bss
        recv = do
            mpkt <- atomically $ tryReadTQueue q
            case mpkt of
              Nothing  -> NSB.recv s 2048 >>= decodeCryptPackets
              Just pkt -> return [pkt]
    conn <- serverConnection serverConfig myCID peerCID oCID send recv
    tid0 <- forkIO $ sender conn
    tid1 <- forkIO $ receiver conn
    tid2 <- forkIO $ resender conn
    setThreadIds conn [tid0,tid1,tid2]
    handshakeServer serverConfig oCID conn
    setConnectionStatus conn Open
    return conn
  where
    tlserr e = E.throwIO $ HandshakeFailed $ show $ errorToAlertDescription e

----------------------------------------------------------------

close :: Connection -> IO ()
close conn = do
    setConnectionStatus conn Closing
    let frames = [ConnectionCloseQUIC NoError 0 ""]
    atomically $ writeTQueue (outputQ conn) $ OutControl RTT1Level frames
    setCloseSent conn
    void $ timeout 100000 $ waitClosed conn -- fixme: timeout
    clearThreads conn
