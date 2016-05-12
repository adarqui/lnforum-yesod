module Handler.SPA (
  getSPAR
) where



import           Import
import           Misc.Widget
import           Socket
import           Yesod.WebSockets



getSPAR :: Handler Html
getSPAR = do

  user_id <- requireAuthId

  webSockets socketHub
  defaultLayout $ do
    setTitle "adarq.org"

    addScriptRemoteBustCache "/static/pure/ln.js"
