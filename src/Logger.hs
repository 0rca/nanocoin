{-# LANGUAGE FlexibleInstances #-}
module Logger where

import Protolude hiding (ask, putText)

import qualified System.Logger as Logger
import System.Logger.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class


instance MonadIO m => MonadLogger (ReaderT Logger m) where
  log lvl m = do
    logger <- ask
    Logger.log logger lvl m

putText :: (MonadIO m, MonadLogger m) => Text -> m ()
putText txt = info (Logger.msg txt)

print :: (MonadIO m, MonadLogger m, Show a) => a -> m ()
print = putText . show
