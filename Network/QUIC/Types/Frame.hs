{-# LANGUAGE OverloadedStrings #-}

module Network.QUIC.Types.Frame where

import Network.QUIC.Imports
import Network.QUIC.Types.Ack
import Network.QUIC.Types.CID
import Network.QUIC.Types.Error

----------------------------------------------------------------

type FrameType = Int

data Direction = Uni | Bidi deriving (Eq, Show)

data Frame = Padding Int
           | Ping
           | Ack AckInfo Delay
           | ResetStream -- fixme
           | StopSending StreamID ApplicationError
           | Crypto Offset CryptoData
           | NewToken Token
           | Stream StreamID Offset StreamData Fin
           | MaxData Int
           | MaxStreamData StreamID Int
           | MaxStreams Direction Int
           | DataBlocked -- fixme
           | StreamDataBlocked -- fixme
           | StreamsBlocked -- fixme
           | NewConnectionID CIDInfo Int
           | RetireConnectionID Int
           | PathChallenge PathData
           | PathResponse PathData
           | ConnectionCloseQUIC TransportError FrameType ReasonPhrase
           | ConnectionCloseApp ApplicationError ReasonPhrase
           | HandshakeDone
           | UnknownFrame Int
           deriving (Eq,Show)

-- | Stream identifier.
type StreamID = Int64
type Delay = Int

type Fin = Bool

type CryptoData = ByteString
type StreamData = ByteString

type Token = ByteString -- to be decrypted
emptyToken :: Token
emptyToken = ""

ackEliciting :: Frame -> Bool
ackEliciting Padding{}             = False
ackEliciting ConnectionCloseQUIC{} = False
ackEliciting ConnectionCloseApp{}  = False
ackEliciting Ack{}                 = False
ackEliciting _                     = True

retransmittable :: Frame -> Bool
retransmittable Padding{}          = False
retransmittable Ack{}              = False
retransmittable _                  = True
