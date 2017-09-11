{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Nanocoin.Network.P2P (
  p2p,
) where

import Protolude hiding (msg, putText)

import Control.Arrow ((&&&))

import Network.Socket (HostName, PortNumber)

import qualified Nanocoin.Block as Block
import qualified Nanocoin.MemPool as MemPool
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Network.Multicast as M
import qualified Nanocoin.Network.Message as Msg
import qualified Nanocoin.Network.Node as Node
import qualified Nanocoin.Network.Peer as Peer
import qualified Nanocoin.Network.RPC as RPC

import System.Logger.Class
import qualified System.Logger as Logger
import Control.Monad.Trans.Reader
import Logger
-------------------------------------------------------------------------------
-- P2P
-------------------------------------------------------------------------------
putText :: (MonadIO m, MonadLogger m) => Text -> m ()
putText txt = info (Logger.msg txt)

-- | Initializes a p2p node and returns a sender function so that
-- the rpc server can broadcast messages to the p2p network
p2p :: Logger.Logger -> Node.NodeState -> IO ()
p2p logger nodeState = do -- TODO: Take buffer size as argument, max size of blockchain
  let (sender,receiver) = Node.nodeSender &&& Node.nodeReceiver $ nodeState
  void $ forkIO $ flip runReaderT logger $ forever $ liftIO receiver >>= -- | Forever handle messages
    -- XXX forkIO here again?
    either putText (handleMsg nodeState . fst)

----------------------------------------------------------------
-- Msg Handling
----------------------------------------------------------------

-- | Main dispatch function to handle all messages received from network
handleMsg :: (MonadIO m, MonadLogger m) => Node.NodeState -> Msg.Msg -> m ()
handleMsg nodeState msg = do

  putText $ "Received Msg: " <> (show msg :: Text)
  let nodeSender = Node.nodeSender nodeState

  case msg of

    Msg.QueryBlockMsg n -> do
      chain <- Node.getBlockChain nodeState
      case find ((==) n . Block.index) chain of
        Nothing    -> putText $
          "No block with index " <> show n
        Just block -> liftIO $ nodeSender $ Msg.BlockMsg block

    Msg.BlockMsg block -> do
      mPrevBlock <- Node.getLatestBlock nodeState
      case mPrevBlock of
        Nothing -> putText "handleMessage: No Genesis block found."
        Just prevBlock -> do
          -- Apply block to world state
          Node.applyBlock nodeState prevBlock block
          -- If the block was successfully applied
          newPrevBlock <- Node.getLatestBlock nodeState
          when (Just block == newPrevBlock) $
            -- Ask if there is a more recent block
            liftIO $ nodeSender $ Msg.QueryBlockMsg (Block.index block + 1)

    Msg.TransactionMsg tx -> do
      ledger <- Node.getLedger nodeState
      -- Verify Signature before adding to MemPool
      case T.verifyTxSignature ledger tx of
        Left err -> print err
        Right _  -> -- Add transaction to mempool
          Node.modifyMemPool_ nodeState $
            MemPool.addTransaction tx
