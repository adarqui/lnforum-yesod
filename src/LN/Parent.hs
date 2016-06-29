{-# LANGUAGE RecordWildCards #-}

module LN.Parent (
  fatherOf_ThreadPostM,
  fatherIdOf_ThreadPostM
--  parentOf_ThreadPostId
) where



import           Api.Params
import           LN.Control
import           Import
import           Lifted
import           LN.T.Membership
import           LN.T.Visibility
import           Model.Misc



fatherOf_ThreadPostM :: UserId -> Entity ThreadPost -> LN.HandlerEff (Maybe (Entity Thread))
fatherOf_ThreadPostM user_id (Entity thread_post_id ThreadPost{..}) = do

  _runDB $ selectFirst [ ThreadId ==. threadPostThreadId ] [ ]



fatherIdOf_ThreadPostM :: UserId -> Entity ThreadPost -> LN.HandlerEff (Maybe ThreadId)
fatherIdOf_ThreadPostM user_id thread_post = fmap entityKey <$> fatherOf_ThreadPostM user_id thread_post



-- parentOf_ThreadPostId :: UserId -> ThreadPostId -> Maybe ThreadPostId
-- parentOf_ThreadPostId _ _ = undefined
