module Parent (
  fatherOf_ThreadPostM,
  fatherIdOf_ThreadPostM
--  parentOf_ThreadPostId
) where



import           Import
import           Api.Params
import           Model.Misc
import           LN.T.Membership
import           LN.T.Visibility



fatherOf_ThreadPostM :: UserId -> Entity ThreadPost -> HandlerEff (Maybe (Entity Thread))
fatherOf_ThreadPostM user_id (Entity thread_post_id ThreadPost{..}) = do

  lift $ runDB $ selectFirst [ ThreadId ==. threadPostThreadId ] [ ]



fatherIdOf_ThreadPostM :: UserId -> Entity ThreadPost -> HandlerEff (Maybe ThreadId)
fatherIdOf_ThreadPostM user_id thread_post = fmap entityKey <$> fatherOf_ThreadPostM user_id thread_post



-- parentOf_ThreadPostId :: UserId -> ThreadPostId -> Maybe ThreadPostId
-- parentOf_ThreadPostId _ _ = undefined
