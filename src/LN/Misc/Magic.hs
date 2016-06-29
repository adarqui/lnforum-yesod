module LN.Misc.Magic (
  isSuper
) where



import           Database.Persist.Sql (fromSqlKey)
import           LN.Import



isSuper :: UserId -> Bool
isSuper user_id = fromSqlKey user_id == 1
