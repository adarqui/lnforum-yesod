{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.User.Internal2 (
  getUsersM,
  getUsersBy_UserIdsM,
  getUsersBy_EverythingM,

  insertUsersM,
  insertUsers_TasksM,
  getUserM,
  getUserMH,
  updateUserM,
  deleteUserM,
  countUsersM,

  getUserStatsM,
  getUserStatM
) where



import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           Model.Prelude
import           Model.Profile.Function
import           Model.User.Function



getUsersM :: UserId -> Handler [Entity User]
getUsersM user_id = do
-- selectListDb [] [] UserId

  sp@StandardParams{..} <- lookupStandardParams

  case spUserIds of

    Just user_ids -> getUsersBy_UserIdsM user_id user_ids sp
    _             -> getUsersBy_EverythingM user_id sp



getUsersBy_UserIdsM :: UserId -> [UserId] -> StandardParams -> Handler [Entity User]
getUsersBy_UserIdsM _ user_ids sp = do

  selectListDb sp [UserId <-. user_ids] [] UserId



getUsersBy_EverythingM :: UserId -> StandardParams -> Handler [Entity User]
getUsersBy_EverythingM _ sp = do

  selectListDb sp [] [] UserId



insertUsersM :: UserId -> UserRequest -> Handler (Entity User)
insertUsersM user_id user_request = do

  -- TODO: FIXME: Fix this
  if (isSuper user_id)

    then do
      ts <- timestampH'
      let
        email_md5 = md5Text (userRequestEmail user_request)
        user = (userRequestToUser user_request) {
            userEmailMD5 = email_md5
          , userCreatedAt = Just ts
        }
      new_user <- insertEntityDb user
      -- TODO FIXME: can't call this because of circular dependency issue, need to figure this out!!
--      insertUsers_TasksM user_id new_user
      return new_user

    else permissionDenied "perms"



insertUsers_TasksM :: UserId -> Entity User -> Handler ()
insertUsers_TasksM _ (Entity new_user_id _) = do

  -- Create a default profile
  --
  void $ insertEntityDb (profileRequestToProfile new_user_id defaultProfileRequest)

  -- Create default settings
  -- TODO
  -- void $ insertEntityDb (settingsRequestToSettings new_user_id defaultSettingsRequest)

  return ()



getUserM :: UserId -> UserId -> Handler (Entity User)
getUserM user_id lookup_user_id = getUserM' user_id (UserId ==. lookup_user_id)



getUserMH :: UserId -> Text -> Handler (Entity User)
getUserMH user_id lookup_user_nick = getUserM' user_id (UserNick ==. lookup_user_nick)



getUserM' :: forall t site val.
             (PersistEntity val, YesodPersist site,
              PersistQuery (YesodPersistBackend site),
              YesodPersistBackend site ~ PersistEntityBackend val) =>
             t -> Filter val -> HandlerT site IO (Entity val)
getUserM' _ q = do
  notFoundMaybe =<< selectFirstDb [q] []



updateUserM :: UserId -> UserId -> UserRequest -> Handler (Entity User)
updateUserM _ lookup_user_id user_request = do

  ts <- timestampH'

  let
    email_md5 = md5Text $ (userRequestEmail user_request)
    User{..} = (userRequestToUser user_request) {
        userEmailMD5 = email_md5
      , userModifiedAt = Just ts
    }

  void $ runDB $ updateWhere
    [ UserId ==. lookup_user_id ]

    [ UserModifiedAt =. userModifiedAt
    , UserNick =. userNick
    , UserDisplayNick =. userDisplayNick
    , UserEmail =. userEmail
    , UserEmailMD5 =. userEmailMD5
    ]

  notFoundMaybe =<< selectFirstDb [ UserId ==. lookup_user_id ] []



deleteUserM :: UserId -> UserId -> Handler ()
deleteUserM user_id lookup_user_id = do

  -- TODO: Fix this
  if (isSuper user_id) || (user_id == lookup_user_id)

    then
      runDB $ delete lookup_user_id

    else
      permissionDenied "perms"




countUsersM :: UserId -> Handler CountResponses
countUsersM _ = do

  StandardParams{..} <- lookupStandardParams

-- case (spOrganizationId, spUserId, spForumId, spBoardId, spThreadId) of

  case spOrganizationId of

    Just _ -> notFound

    Nothing -> do
      n <- countDb [ UserActive ==. True ]
      return $ CountResponses [CountResponse 0 (fromIntegral n)]




getUserStatsM :: UserId -> Handler UserSanitizedStatResponses
getUserStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spUserIds of

    _ -> notFound



getUserStatM :: UserId -> UserId -> Handler UserSanitizedStatResponse
getUserStatM _ lookup_user_id = do

  (a,b,c,d) <- qUserStats lookup_user_id

  let (threads,thread_posts,resources,leurons) = (E.unValue a, E.unValue b, E.unValue c, E.unValue d)

  return $ UserSanitizedStatResponse {
    userSanitizedStatResponseUserId = keyToInt64 lookup_user_id,
    userSanitizedStatResponseThreads = threads,
    userSanitizedStatResponseThreadPosts = thread_posts,
    userSanitizedStatResponseRespect = 0,
    userSanitizedStatResponseResources = resources,
    userSanitizedStatResponseLeurons = leurons,
    userSanitizedStatResponseWorkouts = 0
  }



qUserStats :: forall site.
    (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
    Key User -> HandlerT site IO (E.Value Int64, E.Value Int64, E.Value Int64, E.Value Int64)
qUserStats user_id = do
  runDB $ do
    (leurons:[]) <- E.select
      $ E.from $ \leuron -> do
      E.where_ $ leuron ^. LeuronUserId E.==. E.val user_id
      return (E.countDistinct $ leuron ^. LeuronId)

    (resources:[]) <- E.select
      $ E.from $ \resource -> do
      E.where_ $ resource ^. ResourceUserId E.==. E.val user_id
      return (E.countDistinct $ resource ^. ResourceId)

    (thread_posts:[]) <- E.select
      $ E.from $ \thread_post -> do
      E.where_ $ thread_post ^. ThreadPostUserId E.==. E.val user_id
      return (E.countDistinct $ thread_post ^. ThreadPostId)

    (threads:[]) <- E.select
      $ E.from $ \thread -> do
      E.where_ $ thread ^. ThreadUserId E.==. E.val user_id
      return (E.countDistinct $ thread ^. ThreadId)

    return (threads :: E.Value Int64, thread_posts :: E.Value Int64, resources :: E.Value Int64, leurons :: E.Value Int64)
