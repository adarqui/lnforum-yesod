{-# LANGUAGE RecordWildCards #-}

module LN.All.View (
  -- Handler
  getViewsR,
  postViewR0,
  getViewR,
  putViewR,
  deleteViewR,

  -- Model/Function
  viewRequestToView,
  viewToResponse,
  viewsToResponses,

  -- Model/Internal
  getViewsM,
  insertViewM,
  getViewM,
  getView_ByThreadPostM,
  getView_ByThreadPostIdM,
  updateViewM,
  deleteViewM
) where



import           LN.All.Prelude
import           LN.T.Ent



--
-- Handler
--

getViewsR :: Handler Value
getViewsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON viewsToResponses $ getViewsM (pure sp) user_id



postViewR0 :: Handler Value
postViewR0 = run $ do
  user_id      <- _requireAuthId
  view_request <- requireJsonBody
  sp           <- lookupStandardParams
  errorOrJSON viewToResponse $ insertViewM (pure sp) user_id view_request



getViewR :: ViewId -> Handler Value
getViewR view_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON viewToResponse $ getViewM user_id view_id



putViewR :: ViewId -> Handler Value
putViewR view_id = run $ do
  user_id      <- _requireAuthId
  view_request <- requireJsonBody
  errorOrJSON viewToResponse $ updateViewM user_id view_id view_request



deleteViewR :: ViewId -> Handler Value
deleteViewR view_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteViewM user_id view_id



getViewStatsR :: Handler Value
getViewStatsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getViewStatsM (pure sp) user_id



getViewStatR :: ViewId -> Handler Value
getViewStatR view_id = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getViewStatM (pure sp) user_id view_id





--
-- Model/Function
--

viewRequestToView :: UserId -> Ent -> Int64 -> ViewRequest -> View
viewRequestToView user_id ent ent_id ViewRequest{..} = View {
  viewEnt            = ent,
  viewEntId          = ent_id,
  viewCount          = viewRequestCount,
  viewCreatedAt      = Nothing,
  viewModifiedAt     = Nothing
}



viewToResponse :: Entity View -> ViewResponse
viewToResponse (Entity view_id View{..}) = ViewResponse {
  viewResponseId         = keyToInt64 view_id,
  viewResponseEnt        = viewEnt,
  viewResponseEntId      = viewEntId,
  viewResponseCount      = viewCount,
  viewResponseCreatedAt  = viewCreatedAt,
  viewResponseModifiedAt = viewModifiedAt
}



viewsToResponses :: [Entity View] -> ViewResponses
viewsToResponses views = ViewResponses {
  viewResponses = map viewToResponse views
}



--
-- Model/Internal
--

getViewsM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity View]
getViewsM m_sp user_id = do
  selectListDbE m_sp [] [] ViewId



insertViewM :: Maybe StandardParams -> UserId -> ViewRequest -> HandlerErrorEff (Entity View)
insertViewM m_sp user_id view_request = do

  case (lookupViewEntMay m_sp) of
    Just (ent, ent_id) -> do
      ts <- timestampH'
      let
        view = (viewRequestToView user_id ent ent_id view_request) { viewCreatedAt = Just ts }

      insertEntityDbE view

    _ -> leftA $ Error_InvalidArguments "ent, ent_id"



getViewM :: UserId -> ViewId -> HandlerErrorEff (Entity View)
getViewM user_id view_id = do
  selectFirstDbE [ViewId ==. view_id] []



getView_ByThreadPostM :: UserId -> Entity ThreadPost -> HandlerErrorEff (Entity View)
getView_ByThreadPostM user_id (Entity thread_post_id _) = getView_ByThreadPostIdM user_id thread_post_id



getView_ByThreadPostIdM :: UserId -> ThreadPostId -> HandlerErrorEff (Entity View)
getView_ByThreadPostIdM user_id thread_post_id = do
  selectFirstDbE [ViewEnt ==. Ent_ThreadPost, ViewEntId ==. thread_post_id'] []
  where
  thread_post_id' = keyToInt64 thread_post_id



updateViewM :: UserId -> ViewId -> ViewRequest -> HandlerErrorEff (Entity View)
updateViewM user_id view_id ViewRequest{..} = do

  ts <- timestampH'

  void $ updateWhereDb
    [ ViewId ==. view_id ]

    [ ViewModifiedAt =. Just ts
    , ViewCount      =. viewRequestCount
    ]

  selectFirstDbE [ViewId ==. view_id] []



incView_ByViewIdM :: UserId -> ViewId -> HandlerErrorEff ()
incView_ByViewIdM user_id view_id = do

  ts <- timestampH'

  void $ updateWhereDb
    [ ViewId ==. view_id ]
    [ ViewModifiedAt  =. Just ts
    , ViewCount      +=. 1
    ]



incView_ByEntM :: UserId -> Ent -> EntId -> HandlerErrorEff ()
incView_ByEntM user_id ent ent_id = do

  ts <- timestampH'

  void $ updateWhereDb
    [ Ent ==. ent, EntId ==. ent_id ]
    [ ViewModifiedAt  =. Just ts
    , ViewCount      +=. 1
    ]



deleteViewM :: UserId -> ViewId -> HandlerErrorEff ()
deleteViewM user_id view_id = do
  deleteWhereDbE [ViewId ==. view_id]
