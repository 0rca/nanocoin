{-# LANGUAGE FlexibleInstances #-}
module Logger where

import qualified System.Logger as Logger
import System.Logger.Class hiding (info)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

instance MonadIO m => MonadLogger (ReaderT Logger m) where
  log lvl m = do
    logger <- ask
    Logger.log logger lvl m
