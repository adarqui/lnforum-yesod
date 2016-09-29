{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module LN.Worker.Internal (
  runWorkerDirectly_AddThreadPostToSet
) where



import qualified Data.ByteString.Char8 as BSC
import qualified Database.Redis        as Redis

import           LN.All.Empty          (emptyM)
import           LN.Control
import           LN.Import
import           LN.Misc.Codec         (keyToInt64)



-- | Need this broken out so we can call it from All/ThreadPost.hs
--
runWorkerDirectly_AddThreadPostToSet :: ThreadId -> ThreadPostId -> HandlerT App IO ()
runWorkerDirectly_AddThreadPostToSet thread_id post_id = do
    void $ ((run $ do
      emptyM
      red <- getsYesod appRed
      let
        thread_id' = keyToInt64 thread_id
        post_id'   = keyToInt64 post_id
      void $ liftIO $ Redis.runRedis red $ Redis.zadd ("thread_posts:"<>(BSC.pack $ show thread_id')) [(fromIntegral post_id', BSC.pack $ show post_id')]
      pure ()) :: Handler ())
