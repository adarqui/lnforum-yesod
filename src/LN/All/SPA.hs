module LN.All.SPA (
  getSPAR,
  getSPALnR
) where



import           LN.All.Prelude
import           LN.Misc.Widget
import           LN.Socket
import           Yesod.WebSockets
import           Text.Blaze.Internal



getSPALnR :: Handler Html
getSPALnR = getSPAR "lnforum.adarq.org" "/static/lnforum.dist/bundle.js"


getSPAR :: MarkupM () -> Text -> Handler Html
getSPAR title file = do

  void $ requireAuthId

  webSockets socketHub

  defaultLayout $ do

    setTitle title

    addScriptRemoteBustCache file
