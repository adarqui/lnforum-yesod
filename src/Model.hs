module Model where



import           ClassyPrelude.Yesod
import           Database.Persist.Quasi
import           Model.Like.DerivePersist
import           Model.Profile.DerivePersist
import           Model.Ent.DerivePersist



-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [
    mkPersist sqlSettings,
    mkDeleteCascade sqlSettings,
    mkMigrate "migrateAll"
  ]
  $(persistFileWith lowerCaseSettings "config/models")
