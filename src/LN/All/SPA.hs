module LN.All.SPA (
  getSPAR
) where



import           LN.All.Prelude
import           Misc.Widget
import           Socket
import           Yesod.WebSockets



getSPAR :: LN.Handler Html
getSPAR = do

  void $ requireAuthId

  webSockets socketHub
  defaultLayout $ do
    setTitle "adarq.org"

    addScriptRemoteBustCache "/static/pure/ln.js"
