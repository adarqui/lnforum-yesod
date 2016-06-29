module LN.Model.Misc (
  dummyId
) where



import Import



dummyId :: forall record. PersistEntity record => Key record
dummyId = v
  where
  (Right v) = keyFromValues [PersistInt64 0]
