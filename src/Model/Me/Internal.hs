module Model.Me.Internal (
  getMeM
) where



import           Api.Params
import           Api.Response
import           Import



getMeM :: UserId -> Handler (Entity User)
getMeM user_id = do
  notFoundMaybe =<< selectFirstDb [ UserId ==. user_id ] []
