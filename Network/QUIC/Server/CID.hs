module Network.QUIC.Server.CID where
import Network.QUIC.Imports (Bytes, shortToString, enc16s)
import Network.QUIC.Socket(ToClientSocket)
import Network.QUIC.Types.CID(CID(..))

data ServerCID = ServerCID {scidBytes :: Bytes, scidToClientSock :: ToClientSocket}
instance Eq ServerCID where
    (ServerCID a _) == (ServerCID b _) = a == b
instance Ord ServerCID where
    (ServerCID a _) <= (ServerCID b _) = a <= b
instance Show ServerCID where
    show (ServerCID cid _) = shortToString (enc16s cid)

serverCIDtoCID :: ServerCID -> CID
serverCIDtoCID (ServerCID a _) = CID a