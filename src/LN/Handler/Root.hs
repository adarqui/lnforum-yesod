module LN.Handler.Root (
  getRootR
) where



import Import



getRootR :: LN.Handler Html
getRootR = do
  muser <- maybeAuth
  defaultLayout $ do
    setTitle "Leuro"
    case muser of
      Nothing -> $(widgetFile "root")
      Just _  -> $(widgetFile "root")
