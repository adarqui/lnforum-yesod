{-# LANGUAGE RecordWildCards #-}

module LN.Parent (
  fatherOf_ThreadPostM,
  fatherIdOf_ThreadPostM
--  parentOf_ThreadPostId
) where



import           LN.Control
import           LN.Import
import           LN.Lifted



fatherOf_ThreadPostM :: UserId -> Entity ThreadPost -> HandlerEff (Maybe (Entity Thread))
fatherOf_ThreadPostM _ (Entity _ ThreadPost{..}) = do

  _runDB $ selectFirst [ThreadId ==. threadPostThreadId, ThreadActive ==. True] [ ]



fatherIdOf_ThreadPostM :: UserId -> Entity ThreadPost -> HandlerEff (Maybe ThreadId)
fatherIdOf_ThreadPostM user_id thread_post = fmap entityKey <$> fatherOf_ThreadPostM user_id thread_post



-- parentOf_ThreadPostId :: UserId -> ThreadPostId -> Maybe ThreadPostId
-- parentOf_ThreadPostId _ _ = undefined
