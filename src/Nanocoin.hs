{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Nanocoin (
  initNode
) where

import Protolude hiding (get, put, ask, log)
import System.Logger.Class
import qualified System.Logger as Logger
-- import qualified System.Logger.Message as Logger

import Web.Scotty

import qualified Key
import qualified Nanocoin.Block as B
import qualified Nanocoin.Ledger as L
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.P2P as P2P
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC

-- | Initializes a node on the network with it's own copy of
-- the blockchain, and invokes a p2p server and an http server.
initNode :: (MonadLogger m, MonadIO m) => Int -> Maybe FilePath -> m ()
initNode rpcPort mKeysPath = do
  let peer = Peer.mkPeer rpcPort

  -- Initialize Node Keys
  keys <- case mKeysPath of
    Nothing -> liftIO Key.newKeyPair
    Just keysPath -> do
      eNodeKeys <- liftIO $ Key.readKeys keysPath
      case eNodeKeys of
        Left error   -> do
          err $ Logger.msg error
          liftIO . die $ show error
        Right keys -> pure keys

  -- Initialize Genesis Block
  genesisBlock <- do
    eKeys <- liftIO $ Key.readKeys "keys/genesis"
    case eKeys of
      Left error   -> do
        err (Logger.msg error)
        liftIO . die $ show error
      Right gkeys -> liftIO $ B.genesisBlock gkeys

  -- Initialize NodeState
  nodeState <- liftIO $ Node.initNodeState peer genesisBlock keys

  -- Fork P2P server
  liftIO . forkIO $ P2P.p2p nodeState
  -- Join network by querying latest block
  liftIO . joinNetwork $ Node.nodeSender nodeState
  -- Run RPC server
  liftIO $ RPC.rpcServer nodeState

-- | Query the network for the latest block
joinNetwork :: Msg.MsgSender -> IO ()
joinNetwork nodeSender = nodeSender $ Msg.QueryBlockMsg 1
