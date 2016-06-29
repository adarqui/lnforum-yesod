module LN.Model.Keys (
  usersResourceKey,
  usersResourceLeuronKey,
  usersZLeuronsKey,
  usersLeuronsKey,
  resourcesKey,
  categoriesKey,
  resourceCategoriesKey
) where



import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text             as T
import           Import
import           LN.Misc.Codec



-- | ln:users:<users_id>:resources:<resource_id>
--
usersResourceKey :: UserId -> ResourceId -> LeuronId -> BSC.ByteString
usersResourceKey = error "not implemented"



-- | ln:users:<users_id>:resources:<resources_id>
--
usersResourceLeuronKey :: UserId -> ResourceId -> LeuronId -> BSC.ByteString
usersResourceLeuronKey = error "not implemeneted"



-- | ln:users:<users_id>:leurons
--
usersLeuronsKey :: UserId -> BSC.ByteString
usersLeuronsKey user_id = BSC.concat ["ln:users:", keyToInt64Sbs user_id, ":leurons"]



-- | ln:users:<users_id>:z:leurons
--
usersZLeuronsKey :: UserId -> BSC.ByteString
usersZLeuronsKey user_id = BSC.concat ["ln:users:", keyToInt64Sbs user_id, ":leurons"]



-- | ln:resources:<resource_id>
--
resourcesKey :: ResourceId -> BSC.ByteString
resourcesKey resource_id = BSC.concat ["ln:resources:", keyToInt64Sbs resource_id]



-- | ln:categories:mathematics:set theory:zermelo
--
categoriesKey :: [Text] -> BSC.ByteString
categoriesKey cat = textToSbsConcat ["ln:categories:", (T.concat $ intersperse ":" $ cat)]



-- | ln:resource_categories:mathematics:set theory:zermelo
--
resourceCategoriesKey :: ResourceId -> [Text] -> BSC.ByteString
resourceCategoriesKey resource_id cat =
  textToSbsConcat ["ln:categories:",
    T.pack $ show $ keyToInt64 resource_id,
    (T.concat $ intersperse ":" $ cat)]
