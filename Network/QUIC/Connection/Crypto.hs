{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QUIC.Connection.Crypto (
    setEncryptionLevel
  , waitEncryptionLevel
  , putOffCrypto
  --
  , getCipher
  , setCipher
  , getTLSMode
  , getApplicationProtocol
  , setNegotiated
  --
  , dropSecrets
  --
  , initializeCoder
  , initializeCoder1RTT
  , updateCoder1RTT
  , getCoder
  , getProtector
  --
  , getCurrentKeyPhase
  , setCurrentKeyPhase
  ) where

import Control.Concurrent.STM
import Network.TLS.QUIC

import Network.QUIC.Connection.Misc
import Network.QUIC.Connection.Types
import Network.QUIC.Connector
import Network.QUIC.Imports
import Network.QUIC.TLS
import Network.QUIC.Types

----------------------------------------------------------------

setEncryptionLevel :: Connection -> EncryptionLevel -> IO ()
setEncryptionLevel conn@Connection{..} lvl = do
    (_, q) <- getSockInfo conn
    atomically $ do
        writeTVar (encryptionLevel connState) lvl
        case lvl of
          HandshakeLevel -> do
              readTVar (pendingQ ! RTT0Level)      >>= mapM_ (prependRecvQ q)
              readTVar (pendingQ ! HandshakeLevel) >>= mapM_ (prependRecvQ q)
          RTT1Level      ->
              readTVar (pendingQ ! RTT1Level)      >>= mapM_ (prependRecvQ q)
          _              -> return ()

putOffCrypto :: Connection -> EncryptionLevel -> ReceivedPacket -> IO ()
putOffCrypto Connection{..} lvl rpkt =
    atomically $ modifyTVar' (pendingQ ! lvl) (rpkt :)

waitEncryptionLevel :: Connection -> EncryptionLevel -> IO ()
waitEncryptionLevel Connection{..} lvl = atomically $ do
    l <- readTVar $ encryptionLevel connState
    check (l >= lvl)

----------------------------------------------------------------

getCipher :: Connection -> EncryptionLevel -> IO Cipher
getCipher Connection{..} lvl = readArray ciphers lvl

setCipher :: Connection -> EncryptionLevel -> Cipher -> IO ()
setCipher Connection{..} lvl cipher = writeArray ciphers lvl cipher

----------------------------------------------------------------

getTLSMode :: Connection -> IO HandshakeMode13
getTLSMode Connection{..} = handshakeMode <$> readIORef negotiated

getApplicationProtocol :: Connection -> IO (Maybe NegotiatedProtocol)
getApplicationProtocol Connection{..} = applicationProtocol <$> readIORef negotiated

setNegotiated :: Connection -> HandshakeMode13 -> Maybe NegotiatedProtocol -> ApplicationSecretInfo -> IO ()
setNegotiated Connection{..} mode mproto appSecInf =
    writeIORef negotiated Negotiated {
        handshakeMode = mode
      , applicationProtocol = mproto
      , applicationSecretInfo = appSecInf
      }

----------------------------------------------------------------

dropSecrets :: Connection -> EncryptionLevel -> IO ()
dropSecrets Connection{..} lvl = writeArray coders lvl initialCoder

----------------------------------------------------------------

initializeCoder :: Connection -> EncryptionLevel -> TrafficSecrets a -> IO ()
initializeCoder conn lvl sec = do
    cipher <- getCipher conn lvl
    let (coder, protector) = genCoder (isClient conn) cipher sec
    writeArray (coders conn) lvl coder
    writeArray (protectors conn) lvl protector

initializeCoder1RTT :: Connection -> TrafficSecrets ApplicationSecret -> IO ()
initializeCoder1RTT conn sec = do
    cipher <- getCipher conn RTT1Level
    let (coder, protector) = genCoder (isClient conn) cipher sec
        coder1 = Coder1RTT coder sec
    writeArray (coders1RTT conn) False coder1
    writeArray (protectors conn) RTT1Level protector
    updateCoder1RTT conn True

updateCoder1RTT :: Connection -> Bool -> IO ()
updateCoder1RTT conn nextPhase = do
    cipher <- getCipher conn RTT1Level
    Coder1RTT _ secN <- readArray (coders1RTT conn) (not nextPhase)
    let secN1 = updateSecret cipher secN
    let (coderN1, _) = genCoder (isClient conn) cipher secN1
    let nextCoder = Coder1RTT coderN1 secN1
    writeArray (coders1RTT conn) nextPhase nextCoder

updateSecret :: Cipher -> TrafficSecrets ApplicationSecret -> TrafficSecrets ApplicationSecret
updateSecret cipher (ClientTrafficSecret cN, ServerTrafficSecret sN) = secN1
  where
    Secret cN1 = nextSecret cipher $ Secret cN
    Secret sN1 = nextSecret cipher $ Secret sN
    secN1 = (ClientTrafficSecret cN1, ServerTrafficSecret sN1)

genCoder :: Bool -> Cipher -> TrafficSecrets a -> (Coder, Protector)
genCoder cli cipher (ClientTrafficSecret c, ServerTrafficSecret s) = (coder, protector)
  where
    txSecret | cli           = Secret c
             | otherwise     = Secret s
    rxSecret | cli           = Secret s
             | otherwise     = Secret c
    txPayloadKey = aeadKey cipher txSecret
    txPayloadIV  = initialVector cipher txSecret
    txHeaderKey  = headerProtectionKey cipher txSecret
    enc = encryptPayload cipher txPayloadKey txPayloadIV
    pro = protectionMask cipher txHeaderKey
    rxPayloadKey = aeadKey cipher rxSecret
    rxPayloadIV  = initialVector cipher rxSecret
    rxHeaderKey  = headerProtectionKey cipher rxSecret
    dec = decryptPayload cipher rxPayloadKey rxPayloadIV
    unp = protectionMask cipher rxHeaderKey
    coder = Coder enc dec
    protector = Protector pro unp

getCoder :: Connection -> EncryptionLevel -> Bool -> IO Coder
getCoder conn RTT1Level k = coder1RTT <$> readArray (coders1RTT conn) k
getCoder conn lvl       _ = readArray (coders conn) lvl

getProtector :: Connection -> EncryptionLevel -> IO Protector
getProtector conn lvl = readArray (protectors conn) lvl

----------------------------------------------------------------

getCurrentKeyPhase :: Connection -> IO (Bool, PacketNumber)
getCurrentKeyPhase Connection{..} = readIORef currentKeyPhase

setCurrentKeyPhase :: Connection -> Bool -> PacketNumber -> IO ()
setCurrentKeyPhase Connection{..} k pn = writeIORef currentKeyPhase (k, pn)
