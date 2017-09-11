{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Protolude hiding (option, ask, log)

import Data.Maybe (fromMaybe)

import Nanocoin (initNode)

import Options.Applicative

import qualified System.Logger as Logger
import System.Logger.Class hiding (info)
import Control.Monad.Trans.Reader

data Config = Config
  { rpcPort      :: Int
  , keysPath     :: Maybe FilePath
  }

defaultConfig :: Config
defaultConfig = Config 3000 Nothing


instance MonadLogger (ReaderT Logger IO) where
  log lvl m = do
    logger <- ask
    Logger.log logger lvl m

type InitNodeType = ReaderT Logger IO ()

main :: IO ()
main = do
    Config rpc mKeys <- execParser (info parser mempty)
    logger <- Logger.create Logger.StdOut
    runReaderT (initNode rpc mKeys) logger
  where
    portParser :: Parser (Maybe Int)
    portParser = optional $
      option auto $ long "rpc-port"
                 <> short 'p'
                 <> metavar "RPC_PORT"

    keysParser :: Parser (Maybe FilePath)
    keysParser = optional $
      strOption $ long "keys"
               <> short 'k'
               <> metavar "KEYS_DIR"

    parser = Config
      <$> (fromMaybe (rpcPort defaultConfig) <$> portParser)
      <*> keysParser
