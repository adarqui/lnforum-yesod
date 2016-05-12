module Handler.Root where

import Import

getRootR :: Handler Html
getRootR = do
  muser <- maybeAuth
  defaultLayout $ do
    setTitle "Leuro"
    case muser of
      Nothing -> $(widgetFile "root")
      Just _  -> $(widgetFile "root")
