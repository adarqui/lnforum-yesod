{-# LANGUAGE RecordWildCards #-}

module LN.All.View (
  -- Handler
  getViewsR,
  getViewR,
  putViewR,

  -- Model/Function
  viewRequestToView,
  viewToResponse,
  viewsToResponses,

  -- Model/Internal
  getViewsM,
  getViewM,
  insertView_ByEntM,
  getView_ByThreadPostM,
  getView_ByThreadPostIdM,
  updateViewM,
  deleteViewM,

  -- Other
  incView_ByViewIdM,
  incView_ByEntM
) where



import           LN.All.Prelude



--
-- Handler
--

getViewsR :: Handler Value
getViewsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON viewsToResponses $ getViewsM (pure sp) user_id



getViewR :: Handler Value
getViewR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON viewToResponse $ getViewM (pure sp) user_id



putViewR :: Handler Value
putViewR = run $ do
  user_id      <- _requireAuthId
  sp           <- lookupStandardParams
  view_request <- requireJsonBody
  errorOrJSON viewToResponse $ updateViewM (pure sp) user_id view_request



--
-- Model/Function
--

viewRequestToView :: UserId -> Ent -> Int64 -> ViewRequest -> View
viewRequestToView _ ent ent_id ViewRequest{..} = View {
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
getViewsM m_sp _ = do
  selectListDbE m_sp [] [] ViewId



insertView_ByEntM :: UserId -> Ent -> Int64 -> HandlerErrorEff (Entity View)
insertView_ByEntM user_id ent ent_id = do

  ts <- timestampH'
  let
    view = (viewRequestToView user_id ent ent_id $ ViewRequest 0) { viewCreatedAt = Just ts }

  insertEntityDbE view



getViewM :: Maybe StandardParams -> UserId -> HandlerErrorEff (Entity View)
getViewM m_sp _ = do

  case (lookupViewEntMay m_sp) of
    Just (ent, ent_id) -> do
      selectFirstDbE [ViewEnt ==. ent, ViewEntId ==. ent_id] []
    _ -> leftA $ Error_InvalidArguments "ent, ent_id"



getView_ByThreadPostM :: UserId -> Entity ThreadPost -> HandlerErrorEff (Entity View)
getView_ByThreadPostM user_id (Entity thread_post_id _) = getView_ByThreadPostIdM user_id thread_post_id



getView_ByThreadPostIdM :: UserId -> ThreadPostId -> HandlerErrorEff (Entity View)
getView_ByThreadPostIdM _ thread_post_id = do
  selectFirstDbE [ViewEnt ==. Ent_ThreadPost, ViewEntId ==. thread_post_id'] []
  where
  thread_post_id' = keyToInt64 thread_post_id



updateViewM :: Maybe StandardParams -> UserId -> ViewRequest -> HandlerErrorEff (Entity View)
updateViewM m_sp _ ViewRequest{..} = do

  case lookupViewEntMay m_sp of
    Just (ent, ent_id) -> do

      ts <- timestampH'

      void $ updateWhereDb
        [ ViewEnt ==. ent, ViewEntId ==. ent_id ]

        [ ViewModifiedAt =. Just ts
        , ViewCount      =. viewRequestCount
        ]

      selectFirstDbE [ViewEnt ==. ent, ViewEntId ==. ent_id] []

    _ -> leftA $ Error_InvalidArguments "ent, ent_id"



incView_ByViewIdM :: UserId -> ViewId -> HandlerErrorEff ()
incView_ByViewIdM _ view_id = do

  ts <- timestampH'

  void $ updateWhereDb
    [ ViewId ==. view_id ]
    [ ViewModifiedAt  =. Just ts
    , ViewCount      +=. 1
    ]

  rightA ()



incView_ByEntM :: UserId -> Ent -> Int64 -> HandlerErrorEff ()
incView_ByEntM _ ent ent_id = do

  ts <- timestampH'

  void $ updateWhereDb
    [ ViewEnt ==. ent, ViewEntId ==. ent_id ]
    [ ViewModifiedAt  =. Just ts
    , ViewCount      +=. 1
    ]

  rightA ()



deleteViewM :: Maybe StandardParams -> UserId -> HandlerErrorEff ()
deleteViewM m_sp _ = do

  case lookupViewEntMay m_sp of
    Just (ent, ent_id) -> do
      deleteWhereDbE [ViewEnt ==. ent, ViewEntId ==. ent_id]

    _ -> leftA $ Error_InvalidArguments "ent, ent_id"
