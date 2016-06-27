module Error (
  notFoundMaybe,
  errorOrJSON,
  errorOrJSONMay,
  permissionDeniedEither,
) where



import Import
import LN.T.Error (ApplicationError)



notFoundMaybe :: forall (m :: * -> *) a. MonadHandler m => Maybe a -> m a
notFoundMaybe mentity = do
  case mentity of
    Nothing       -> notFound
    (Just entity) -> return entity



errorOrJSONMay
  :: (MonadHandler m, ToJSON b) => (a -> b) -> m (Maybe a) -> m Value
errorOrJSONMay trfm go = notFoundMaybe =<< (fmap (toJSON . trfm)) <$> go



errorOrJSON
  :: (MonadHandler m, ToJSON b) => (a -> b) -> m (Either ApplicationError a) -> m Value
errorOrJSON trfm go = do
  e <- (fmap (toJSON . trfm)) <$> go
  case e of
    Left err -> permissionDenied $ tshow $ toJSON err
    Right v  -> pure v



permissionDeniedEither :: forall (m :: * -> *) b. MonadHandler m => Either Text b -> m b
permissionDeniedEither lr = do
  case lr of
    Left err -> permissionDenied err
    Right b  -> return b
