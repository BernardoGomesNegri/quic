{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QUIC.TLS (
    clientHandshaker
  , serverHandshaker
  ) where

import Data.Default.Class
import Network.TLS hiding (Version)
import Network.TLS.QUIC

import Network.QUIC.Config
import Network.QUIC.Parameters
import Network.QUIC.Types

sessionManager :: SessionEstablish -> SessionManager
sessionManager establish = SessionManager {
    sessionEstablish      = establish
  , sessionResume         = \_ -> return Nothing
  , sessionResumeOnlyOnce = \_ -> return Nothing
  , sessionInvalidate     = \_ -> return ()
  }

clientHandshaker :: QUICCallbacks
                 -> ClientConfig
                 -> Version
                 -> AuthCIDs
                 -> SessionEstablish
                 -> Bool
                 -> IO ()
clientHandshaker callbacks ClientConfig{..} ver myAuthCIDs establish use0RTT =
    tlsQUICClient cparams callbacks
  where
    cparams = (defaultParamsClient ccServerName "") {
        clientShared            = cshared
      , clientHooks             = hook
      , clientSupported         = supported
      , clientDebug             = debug
      , clientWantSessionResume = resumptionSession ccResumption
      , clientEarlyData         = if use0RTT then Just "" else Nothing
      }
    convTP = onTransportParametersCreated ccHooks
    params = convTP $ setCIDsToParameters myAuthCIDs ccParameters
    convExt = onTLSExtensionCreated ccHooks
    cshared = def {
        sharedValidationCache = if ccValidate then
                                  def
                                else
                                  ValidationCache (\_ _ _ -> return ValidationCachePass) (\_ _ _ -> return ())
      , sharedHelloExtensions = convExt $ parametersToExtensionRaw ver params
      , sharedSessionManager = sessionManager establish
      }
    hook = def {
        onSuggestALPN = ccALPN ver
      }
    supported = defaultSupported {
        supportedCiphers  = ccCiphers
      , supportedGroups   = ccGroups
      }
    debug = def {
        debugKeyLogger = ccKeyLog
      }

parametersToExtensionRaw :: Version -> Parameters -> [ExtensionRaw]
parametersToExtensionRaw ver params = [ExtensionRaw tpId eParams]
  where
    tpId = extensionIDForTtransportParameter ver
    eParams = encodeParameters params

serverHandshaker :: QUICCallbacks
                 -> ServerConfig
                 -> Version
                 -> IO Parameters
                 -> IO ()
serverHandshaker callbacks ServerConfig{..} ver getParams =
    tlsQUICServer sparams callbacks
  where
    sparams = def {
        serverShared    = sshared
      , serverHooks     = hook
      , serverSupported = supported
      , serverDebug     = debug
      , serverEarlyDataSize = if scUse0RTT then quicMaxEarlyDataSize else 0
      }
    convTP = onTransportParametersCreated scHooks
    convExt = onTLSExtensionCreated scHooks
    sshared = def {
        sharedCredentials     = scCredentials
      , sharedSessionManager  = scSessionManager
      }
    hook = def {
        onALPNClientSuggest = case scALPN of
          Nothing -> Nothing
          Just io -> Just $ io ver
      , onEncryptedExtensionsCreating = \exts0 -> do
            params <- getParams
            let exts = convExt $ parametersToExtensionRaw ver $ convTP params
            return $ exts ++ exts0
      }
    supported = def {
        supportedVersions = [TLS13]
      , supportedCiphers  = scCiphers
      , supportedGroups   = scGroups
      }
    debug = def {
        debugKeyLogger = scKeyLog
      }
