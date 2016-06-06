{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Thread.Function (
  threadRequestToThread,
  threadToResponse,
  threadsToResponses,
) where



import           Model.Prelude



threadRequestToThread :: UserId -> BoardId -> ThreadRequest -> Thread
threadRequestToThread user_id board_id ThreadRequest{..} = Thread {
  threadUserId      = user_id,
  threadBoardId     = board_id,
  threadName        = toPrettyUrl threadRequestDisplayName,
  threadDisplayName = threadRequestDisplayName,
  threadDescription = threadRequestDescription,
  threadSticky      = threadRequestSticky,
  threadLocked      = threadRequestLocked,
  threadPoll        = threadRequestPoll,
  threadIcon        = threadRequestIcon,
  threadTags        = threadRequestTags,
  threadActive      = True,
  threadGuard       = threadRequestGuard,
  threadCreatedAt   = Nothing,
  threadModifiedBy  = Nothing,
  threadModifiedAt  = Nothing,
  threadActivityAt  = Nothing
}



threadToResponse :: Entity Thread -> ThreadResponse
threadToResponse (Entity thread_id Thread{..}) = ThreadResponse {
  threadResponseId          = keyToInt64 thread_id,
  threadResponseUserId      = keyToInt64 threadUserId,
  threadResponseBoardId     = keyToInt64 threadBoardId,
  threadResponseName        = threadName,
  threadResponseDisplayName = threadDisplayName,
  threadResponseDescription = threadDescription,
  threadResponseSticky      = threadSticky,
  threadResponseLocked      = threadLocked,
  threadResponsePoll        = threadPoll,
  threadResponseIcon        = threadIcon,
  threadResponseTags        = threadTags,
  threadResponseActive      = threadActive,
  threadResponseGuard       = threadGuard,
  threadResponseCreatedAt   = threadCreatedAt,
  threadResponseModifiedBy  = fmap keyToInt64 threadModifiedBy,
  threadResponseModifiedAt  = threadModifiedAt,
  threadResponseActivityAt  = threadActivityAt
}



threadsToResponses :: [Entity Thread] -> ThreadResponses
threadsToResponses threads = ThreadResponses {
  threadResponses = map threadToResponse threads
}
