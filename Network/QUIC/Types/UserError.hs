module Network.QUIC.Types.UserError where

import qualified Control.Exception as E

import Network.QUIC.Imports
import Network.QUIC.Types.Error
import Network.QUIC.Types.Packet

data QUICError = PacketCannotBeDecrypted
               | VersionIsUnknown Word32
               | TransportErrorOccurs TransportError ReasonPhrase
               | ApplicationErrorOccurs ApplicationError ReasonPhrase
               | ConnectionIsClosed
               | ConnectionIsNotOpen
               | HandshakeFailed String
               | NoVersionIsSpecified
               | VersionNegotiationFailed
               | BadThingHappen E.SomeException
               deriving (Show)

instance E.Exception QUICError

data InternalControl = NextVersion Version
                     | MustNotReached
                     deriving (Eq, Show)

instance E.Exception InternalControl
