module LN.All.SPA (
  getSPAR,
  getSPALnR,
  getSPALNotesR
) where



import           LN.All.Prelude
import           LN.Misc.Widget
import           LN.Socket
import           Yesod.WebSockets
import           Text.Blaze.Internal



getSPALnR :: Handler Html
getSPALnR = getSPAR "adarq.org" "/static/ln.dist/all.min.js"



getSPALNotesR :: Handler Html
getSPALNotesR = getSPAR "adarq.org" "/static/lnotes.dist/all.min.js"



getSPAR :: MarkupM () -> Text -> Handler Html
getSPAR title file = do

  void $ requireAuthId

  webSockets socketHub

  defaultLayout $ do

    setTitle title

    addScriptRemoteBustCache file
