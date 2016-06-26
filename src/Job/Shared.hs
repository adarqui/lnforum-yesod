{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Job.Shared (
  BgReader (..),
  bgConnect,
  bgRunEnq,
  bgRunDeq,
  bgEnq,
  bgDeq
) where



import           All.Organization
import           Api.Params
import           Control
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.List                    (nub)
import           Import.NoFoundation
import           LN.T.Internal.Types
import           LN.T.Job
import           Misc.Codec                   (int64ToKey')
import           Model.Misc
import           Network.AMQP
import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)



data BgReader = BgReader {
  bgConn :: Connection,
  bgChan :: Channel,
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

  declareQueue bgChan $ newQueue {queueName = tshow q}
  declareExchange bgChan $ newExchange {exchangeName = bgExchg, exchangeType = "direct"}
  bindQueue bgChan (tshow bgQueue) bgExchg (tshow bgQueue)

  void $ runReaderT go bg_reader

  closeConnection bgConn



bgRunDeq :: Queue -> ReaderT BgReader IO a -> IO ()
bgRunDeq q go = do

  bg_reader@BgReader{..} <- bgConnect q

  void $ runReaderT go bg_reader



bgEnq :: BL.ByteString -> ReaderT BgReader IO ()
bgEnq v = do
  BgReader{..} <- ask
  liftIO $ publishMsg bgChan (tshow bgExchg) (tshow bgQueue) $
    newMsg {
      msgBody         = v,
      msgDeliveryMode = Just Persistent
    }



bgDeq :: ((Message, Envelope) -> IO ()) -> ReaderT BgReader IO ()
bgDeq cb = do
  BgReader{..} <- ask
  liftIO $ void $ consumeMsgs bgChan (tshow bgQueue) Ack cb
