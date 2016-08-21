{-# LANGUAGE RecordWildCards #-}

module LN.All.Star (
  -- handler
  getStarsR,
  postStarR0,
  getStarR,
  putStarR,
  deleteStarR,

  getStarStatsR,
  getStarStatR,

  -- model/functions
  starRequestToStar,
  starToResponse,
  starsToResponses,

  -- model/internal
  getStarsM,
  insertStarM,
  getStarM,
  getStar_ByThreadPostM,
  getStar_ByThreadPostIdM,
  updateStarM,
  deleteStarM,

  getStarStatsM,
  getStarStatM
) where



import           LN.All.Prelude



--
-- LN.Handler
--

getStarsR :: Handler Value
getStarsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON starsToResponses $ getStarsM (pure sp) user_id



postStarR0 :: Handler Value
postStarR0 = run $ do
  user_id <- _requireAuthId
  star_request <- requireJsonBody
  sp <- lookupStandardParams
  errorOrJSON starToResponse $ insertStarM (pure sp) user_id star_request



getStarR :: StarId -> Handler Value
getStarR star_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON starToResponse $ getStarM user_id star_id



putStarR :: StarId -> Handler Value
putStarR star_id = run $ do
  user_id      <- _requireAuthId
  star_request <- requireJsonBody
  errorOrJSON starToResponse $ updateStarM user_id star_id star_request



deleteStarR :: StarId -> Handler Value
deleteStarR star_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteStarM user_id star_id



getStarStatsR :: Handler Value
getStarStatsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getStarStatsM (pure sp) user_id



getStarStatR :: StarId -> Handler Value
getStarStatR star_id = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getStarStatM (pure sp) user_id star_id





--
-- LN.Model/Function
--

starRequestToStar :: UserId -> Ent -> Int64 -> StarRequest -> Star
starRequestToStar user_id ent ent_id StarRequest{..} = Star {
  starUserId         = user_id,
  starEnt            = ent,
  starEntId          = ent_id,
  starReason         = starRequestReason,
  starActive         = True,
  starGuard          = starRequestGuard,
  starCreatedAt      = Nothing,
  starModifiedAt     = Nothing
}



starToResponse :: Entity Star -> StarResponse
starToResponse (Entity star_id Star{..}) = StarResponse {
  starResponseId         = keyToInt64 star_id,
  starResponseUserId     = keyToInt64 starUserId,
  starResponseEnt        = starEnt,
  starResponseEntId      = starEntId,
  starResponseReason     = starReason,
  starResponseActive     = starActive,
  starResponseGuard      = starGuard,
  starResponseCreatedAt  = starCreatedAt,
  starResponseModifiedAt = starModifiedAt
}



starsToResponses :: [Entity Star] -> StarResponses
starsToResponses stars = StarResponses {
  starResponses = map starToResponse stars
}



--
-- LN.Model/Internal
--

getStarsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity Star]
getStarsM m_sp user_id = do
  selectListDbE m_sp [StarUserId ==. user_id, StarActive ==. True] [] StarId



insertStarM :: Maybe StandardParams -> UserId -> StarRequest -> HandlerErrorEff (Entity Star)
insertStarM m_sp user_id star_request = do

  case (lookupStarEntMay m_sp) of
    Just (ent, ent_id) -> do

      ts <- timestampH'
      let
        star = (starRequestToStar user_id ent ent_id star_request) { starCreatedAt = Just ts }

      insertEntityDbE star

    _ -> leftA $ Error_InvalidArguments "ent, ent_id"



getStarM :: UserId -> StarId -> HandlerErrorEff (Entity Star)
getStarM user_id star_id = do
  selectFirstDbE [StarId ==. star_id, StarUserId ==. user_id, StarActive ==. True] []



getStar_ByThreadPostM :: UserId -> Entity ThreadPost -> HandlerErrorEff (Entity Star)
getStar_ByThreadPostM user_id (Entity thread_post_id _) = getStar_ByThreadPostIdM user_id thread_post_id



getStar_ByThreadPostIdM :: UserId -> ThreadPostId -> HandlerErrorEff (Entity Star)
getStar_ByThreadPostIdM user_id thread_post_id = do
  selectFirstDbE [StarUserId ==. user_id, StarEnt ==. Ent_ThreadPost, StarEntId ==. thread_post_id', StarActive ==. True] []
  where
  thread_post_id' = keyToInt64 thread_post_id



updateStarM :: UserId -> StarId -> StarRequest -> HandlerErrorEff (Entity Star)
updateStarM user_id star_id StarRequest{..} = do

  ts <- timestampH'

  void $ updateWhereDb
    [ StarId ==. star_id, StarUserId ==. user_id ]

    [ StarModifiedAt =. Just ts
    , StarReason     =. starRequestReason
    ]

  selectFirstDbE [ StarId ==. star_id, StarActive ==. True ] []



deleteStarM :: UserId -> StarId -> HandlerErrorEff ()
deleteStarM user_id star_id = do
  deleteWhereDbE [ StarUserId ==. user_id, StarId ==. star_id, StarActive ==. True ]



getStarStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff StarStatResponses
getStarStatsM _ _ = leftA Error_NotImplemented



getStarStatM :: Maybe StandardParams -> UserId -> StarId -> HandlerErrorEff StarStatResponse
getStarStatM m_sp user_id _ = do

  case (lookupSpMay m_sp spThreadPostId) of
    Just thread_post_id -> getStarStat_ByThreadPostIdM user_id thread_post_id
    _                   -> leftA $ Error_InvalidArguments "thread_post_id"



getStarStat_ByThreadPostIdM :: UserId -> ThreadPostId -> HandlerErrorEff StarStatResponse
getStarStat_ByThreadPostIdM _ thread_post_id = do

  stars <- selectListDb Nothing [StarEnt ==. Ent_ThreadPost, StarEntId ==. i64, StarActive ==. True] [] StarId

  rightA $ StarStatResponse {
    starStatResponseEnt   = Ent_ThreadPost,
    starStatResponseEntId = i64,
    starStatResponseStars = fromIntegral $ length stars
  }
  where
  i64 = keyToInt64 thread_post_id
