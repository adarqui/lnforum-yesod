{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.Job.Shared (
  BgReader (..),
  bgConnect,
  bgRunEnq,
  bgRunDeq,
  bgEnq,
  bgDeq
) where



import           Data.Aeson
import           LN.Import.NoFoundation
import           LN.T.Job
import           Network.AMQP



data BgReader = BgReader {
  bgConn  :: Connection,
  bgChan  :: Channel,
  bgExchg :: Text,
  bgQueue :: Queue
}



bgConnect :: Queue -> IO BgReader
bgConnect q = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  pure $ BgReader {
    bgConn  = conn,
    bgChan  = chan,
    bgExchg = "ln",
    bgQueue = q
  }



bgRunEnq :: Queue -> ReaderT BgReader IO a -> IO ()
bgRunEnq q go = do
  bg_reader@BgReader{..} <- bgConnect q

  void $ declareQueue bgChan $ newQueue {queueName = tshow bgQueue}
  declareExchange bgChan $ newExchange {exchangeName = bgExchg, exchangeType = "direct"}
  bindQueue bgChan (tshow bgQueue) bgExchg (tshow bgQueue)

  void $ runReaderT go bg_reader
  closeConnection bgConn



bgRunDeq :: Queue -> ReaderT BgReader IO a -> IO ()
bgRunDeq q go = do
  bg_reader@BgReader{..} <- bgConnect q
  void $ runReaderT go bg_reader



bgEnq :: ToJSON v => v -> ReaderT BgReader IO ()
bgEnq v = do
  BgReader{..} <- ask
  liftIO $ putStrLn $ tshow bgQueue
  liftIO $ publishMsg bgChan bgExchg (tshow bgQueue) $
    newMsg {
      msgBody         = encode v,
      msgDeliveryMode = Just Persistent
    }



bgDeq :: ((Message, Envelope) -> IO ()) -> ReaderT BgReader IO ()
bgDeq cb = do
  BgReader{..} <- ask
  liftIO $ void $ consumeMsgs bgChan (tshow bgQueue) Ack cb
