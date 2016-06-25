module Error (
  notFoundMaybe,
  errorOrJSON,
  permissionDeniedEither,
) where



import Import



notFoundMaybe :: forall (m :: * -> *) a. MonadHandler m => Maybe a -> m a
notFoundMaybe mentity = do
  case mentity of
    Nothing       -> notFound
    (Just entity) -> return entity



errorOrJSON trfm go = notFoundMaybe =<< (fmap (toJSON . trfm)) <$> go



permissionDeniedEither :: forall (m :: * -> *) b. MonadHandler m => Either Text b -> m b
permissionDeniedEither lr = do
  case lr of
    Left err -> permissionDenied err
    Right b  -> return b
