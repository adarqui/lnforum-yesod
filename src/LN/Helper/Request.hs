-- Stolen from Carnival.
--

module LN.Helper.Request (
  allowCrossOrigin,
  fromMaybe404
) where



import           LN.Control
import           Import



lookupUtf8Header :: HeaderName -> LN.HandlerEff (Maybe Text)
lookupUtf8Header headerName = pure . fmap decodeUtf8 =<< lookupHeader headerName



allowCrossOrigin :: LN.HandlerEff ()
allowCrossOrigin = do
  mo <- lookupUtf8Header "Origin"
  mrh <- lookupUtf8Header "Access-Control-Req-Headers"

  case mo of
    Just o  -> addHeader "Access-Control-Allow-Origin" o
    Nothing -> pure ()

  case mrh of
    Just rh -> addHeader "Access-Control-Allow-Headers" rh
    Nothing -> pure ()

  addHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"
  addHeader "Access-Control-Allow-Credentials" "true"



fromMaybe404 :: LN.HandlerEff (Maybe a) -> LN.HandlerEff a
fromMaybe404 f = maybe notFound pure =<< f
