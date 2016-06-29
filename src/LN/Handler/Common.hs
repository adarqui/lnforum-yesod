-- | Common handler functions.
module LN.Handler.Common (
  getFaviconR,
  getRobotsR
) where



import           Data.FileEmbed (embedFile)
import           Import



-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: LN.Handler TypedContent
getFaviconR = pure $ TypedContent "image/x-icon"
                   $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: LN.Handler TypedContent
getRobotsR = pure $ TypedContent typePlain
                  $ toContent $(embedFile "config/robots.txt")
