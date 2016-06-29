module LN.Misc.Widget (
  addScriptRemoteBustCache
) where

import qualified Data.Text             as T (pack)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Import

-- addScriptRemoteBustCache :: Text -> HandlerEff Html
addScriptRemoteBustCache :: forall (m :: * -> *). MonadWidget m => Text -> m ()
addScriptRemoteBustCache url = do
  ts <- liftIO $ getPOSIXTime
  addScriptRemote (url <> ("?" :: Text) <> (T.pack $ show ts))
