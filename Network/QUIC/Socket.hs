{-# LANGUAGE CPP #-}
module Network.QUIC.Socket where

import Control.Concurrent
import Control.Concurrent.STM.TQueue(TQueue, readTQueue, writeTQueue, newTQueueIO)
import Control.Concurrent.STM(atomically)
import Data.IP hiding (addr)
import qualified GHC.IO.Exception as E
import Network.Socket
import Network.Socket.ByteString as NSB (recv)
import System.IO
import qualified System.IO.Error as E
import qualified UnliftIO.Exception as E
import Data.ByteString as B (ByteString, length, take)
import Network.ByteOrder (Buffer, Word8)
import Foreign.Ptr(Ptr)

-- The idea is that the server keeps a map of SockAddr to these, and anything that enters is matched to a ToClientSocket and written to its queue.
-- It is deliberately not transferred during migrations
data ToClientSocket = ToClientSocket {
    readQ :: TQueue ByteString,
    writeQ :: TQueue (Buffer,Int),
    peer :: SockAddr
}

class SocketLike a where
    recv :: a -> Int -> IO B.ByteString
    send :: a -> Ptr Word8 -> Int -> IO Int

instance SocketLike Socket where
    recv = NSB.recv
    send = sendBuf

instance SocketLike ToClientSocket where
    recv (ToClientSocket {readQ = ourReadQ}) n = do
        -- recv can send smaller stuff than asked, but never larger
        bs <- atomically $ readTQueue ourReadQ
        if B.length bs > n then
            return $ B.take n bs
        else
            return bs

    send (ToClientSocket {writeQ = ourWriteQ}) wordptr n =
        atomically (writeTQueue ourWriteQ (wordptr,n)) >> return n

newToClientSocket :: SockAddr -> IO ToClientSocket
newToClientSocket addr = do
    entryQ <- newTQueueIO
    exitQ <- newTQueueIO
    return $ ToClientSocket entryQ exitQ addr

sockAddrFamily :: SockAddr -> Family
sockAddrFamily SockAddrInet{}  = AF_INET
sockAddrFamily SockAddrInet6{} = AF_INET6
sockAddrFamily _               = error "sockAddrFamily"

anySockAddr :: SockAddr -> SockAddr
anySockAddr (SockAddrInet p _)      = SockAddrInet  p 0
anySockAddr (SockAddrInet6 p f _ s) = SockAddrInet6 p f (0,0,0,0) s
anySockAddr _                       = error "anySockAddr"

udpServerListenSocket :: (IP, PortNumber) -> IO (Socket, SockAddr)
udpServerListenSocket ip = E.bracketOnError open close $ \s -> do
    setSocketOption s ReuseAddr 1
    withFdSocket s setCloseOnExecIfNeeded
    -- setSocketOption s IPv6Only 1 -- fixme
    bind s sa
    return (s,sa)
  where
    sa     = toSockAddr ip
    family = sockAddrFamily sa
    open   = socket family Datagram defaultProtocol

-- | Left here for compatibility purposes with client. DO NOT USE ON SERVER. See https://github.com/kazu-yamamoto/quic/pull/30#issuecomment-1000676443
udpClientConnectedSocket' :: SockAddr -> SockAddr -> IO Socket
udpClientConnectedSocket' mysa peersa = E.bracketOnError open close $ \s -> do
    putStrLn "udpServerConnectedSocket" >> hFlush stdout
    setSocketOption s ReuseAddr 1
    withFdSocket s setCloseOnExecIfNeeded
    -- bind and connect is not atomic
    -- So, bind may results in EADDRINUSE
    putStrLn ("bind to " ++ show anysa ++ "...") >> hFlush stdout
    bind s anysa      -- (UDP, *:13443, *:*)
       `E.catch` postphone (bind s anysa)
    putStrLn ("bind done") >> hFlush stdout
    putStrLn ("connect to " ++ show peersa ++ "...") >> hFlush stdout
    connect s peersa  -- (UDP, 127.0.0.1:13443, pa:pp)
    putStrLn ("connect done") >> hFlush stdout
    getSocketName s >>= print
    hFlush stdout
    getPeerName s >>= print
    hFlush stdout
    return s
  where
    postphone action e
      | E.ioeGetErrorType e == E.ResourceBusy = threadDelay 10000 >> action
      | otherwise                             = E.throwIO e
    anysa  = anySockAddr mysa
    family = sockAddrFamily mysa
    open   = socket family Datagram defaultProtocol

udpClientSocket :: HostName -> ServiceName -> IO (Socket,SockAddr)
#ifdef mingw32_HOST_OS
udpClientSocket = udpClientConnectedSocket
#else
udpClientSocket host port = do
    addr <- head <$> getAddrInfo (Just hints) (Just host) (Just port)
    E.bracketOnError (openSocket addr) close $ \s -> do
        --print "bound socket on client"
        let sa = addrAddress addr
        return (s,sa)
 where
    hints = defaultHints { addrSocketType = Datagram }
#endif

udpClientConnectedSocket :: HostName -> ServiceName -> IO (Socket,SockAddr)
udpClientConnectedSocket host port = do
    addr <- head <$> getAddrInfo (Just hints) (Just host) (Just port)
    E.bracketOnError (openSocket addr) close $ \s -> do
        let sa = addrAddress addr
        connect s sa
        --print "connected socket"
        return (s,sa)
 where
    hints = defaultHints { addrSocketType = Datagram }

udpNATRebindingSocket :: SockAddr -> IO Socket
udpNATRebindingSocket peersa = E.bracketOnError open close $ \s ->
    return s
  where
    family = sockAddrFamily peersa
    open = socket family Datagram defaultProtocol

udpNATRebindingConnectedSocket :: SockAddr -> IO Socket
udpNATRebindingConnectedSocket peersa = E.bracketOnError open close $ \s -> do
    connect s peersa
    return s
  where
    family = sockAddrFamily peersa
    open = socket family Datagram defaultProtocol
