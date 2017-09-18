{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin (
  initNode
) where

import Protolude hiding (get, put)

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

-- | Initialize Node Keys
initKeys :: Maybe FilePath -> IO Key.KeyPair
initKeys Nothing = Key.newKeyPair
initKeys (Just keysPath) = do
    eNodeKeys <- Key.readKeys keysPath
    case eNodeKeys of
      Left err   -> die $ show err
      Right keys -> pure keys

-- | Initializes a node on the network with it's own copy of
-- the blockchain, and invokes a p2p server and an http server.
initNode' :: Int -> Key.KeyPair -> IO ()
initNode' rpcPort keys = do
  let peer = Peer.mkPeer rpcPort

  -- Initialize Genesis Block
  genesisBlock <- do
    eKeys <- Key.readKeys "keys/genesis"
    case eKeys of
      Left err   -> die $ show err
      Right gkeys -> B.genesisBlock gkeys

  -- Initialize NodeState
  nodeState <- Node.initNodeState peer genesisBlock keys

  -- Fork P2P server
  forkIO $ P2P.p2p nodeState
  -- Join network by querying latest block
  joinNetwork $ Node.nodeSender nodeState
  -- Run RPC server
  RPC.rpcServer nodeState

-- | Initializes a node on the network with it's own copy of
-- the blockchain, and invokes a p2p server and an http server.
initNode :: Int -> Maybe FilePath -> IO ()
initNode rpcPort mKeysPath = initNode' rpcPort =<< initKeys mKeysPath

-- | Query the network for the latest block
joinNetwork :: Msg.MsgSender -> IO ()
joinNetwork nodeSender = nodeSender $ Msg.QueryBlockMsg 1
