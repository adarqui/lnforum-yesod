module Api.Response (
  notFoundMaybe,
  permissionDeniedEither
) where



import Import
import Data.Text (Text)



notFoundMaybe :: forall (m :: * -> *) a. MonadHandler m => Maybe a -> m a
notFoundMaybe mentity = do
  case mentity of
    Nothing       -> notFound
    (Just entity) -> return entity



permissionDeniedEither :: forall (m :: * -> *) b. MonadHandler m => Either Text b -> m b
permissionDeniedEither lr = do
  case lr of
    Left err -> permissionDenied err
    Right b  -> return b
