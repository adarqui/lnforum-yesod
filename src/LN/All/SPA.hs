module LN.All.SPA (
  getSPAR
) where



import           LN.All.Prelude
import           LN.Misc.Widget
import           LN.Socket
import           Yesod.WebSockets



getSPAR :: Handler Html
getSPAR = do

  void $ requireAuthId

  webSockets socketHub
  defaultLayout $ do
    setTitle "adarq.org"

    addScriptRemoteBustCache "/static/pure/ln.js"
