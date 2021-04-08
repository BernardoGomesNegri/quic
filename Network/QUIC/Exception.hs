{-# LANGUAGE ScopedTypeVariables #-}

module Network.QUIC.Exception (
    handleLogT
  , handleLogR
  , handleLogE
  , handleLogRun
  , handleLogUnit
  ) where

import qualified Control.Exception as E
import qualified GHC.IO.Exception as E
import qualified System.IO.Error as E

import Network.QUIC.Logger
import Network.QUIC.Types

handleLogRun :: DebugLogger -> IO () -> IO ()
handleLogRun logAction action = E.handle handler action
  where
    handler :: E.SomeException -> IO ()
    handler se
      | Just E.ThreadKilled     <- E.fromException se = return ()
      | Just ConnectionIsClosed <- E.fromException se = return ()
      | otherwise                                     = logAction $ bhow se

handleLogT :: DebugLogger -> IO () -> IO ()
handleLogT logAction action = E.handle handler action
  where
    handler se@(E.SomeException e)
      | Just E.ThreadKilled     <- E.fromException se = return ()
      | Just BreakForever       <- E.fromException se = return ()
      | otherwise                                     = do
            _ <- E.throwIO e
            logAction $ bhow se

-- Log and return a value
handleLogR :: forall a . (Builder -> IO a) -> IO a -> IO a
handleLogR logAction action = E.handle handler action
  where
    handler :: E.SomeException -> IO a
    handler se = logAction $ bhow se

handleLogUnit :: (Builder -> IO ()) -> IO () -> IO ()
handleLogUnit logAction action = E.handle handler action
  where
    handler :: E.SomeException -> IO ()
    handler se
      | Just E.ThreadKilled     <- E.fromException se     = return ()
      -- threadWait: invalid argument (Bad file descriptor)
      | otherwise = case E.fromException se of
          Just e | E.ioeGetErrorType e == E.InvalidArgument -> return ()
          _                                                 -> logAction $ bhow se

-- Log and throw an exception
handleLogE :: (Builder -> IO ()) -> IO a -> IO a
handleLogE logAction action = E.handle handler action
  where
    handler :: E.SomeException -> IO a
    handler se@(E.SomeException e) = do
        logAction $ bhow se
        E.throwIO e
