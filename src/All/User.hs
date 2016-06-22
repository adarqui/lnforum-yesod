{-# LANGUAGE RecordWildCards #-}

module All.User (
  -- Handler
  getUsersR,
  postUserR0,
  getUserR,
  getUserH,
  putUserR,
  deleteUserR,
  getCountUsersR,
  getUserStatsR,
  getUserStatR,


  -- Model/Function
  profileNameToNick,
  userRequestToUser,
  userToResponse,
  usersToResponses,
  userToSanitizedResponse,
  usersToSanitizedResponses,
  validateUserRequest,

  -- Model/Internal
  getUsersM,
  getUsers_ByUserIdsM,
  getUsers_ByEverythingM,
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



import           All.Prelude
import           Data.Char           (isAlphaNum)
import qualified Data.Text           as T (filter, toLower)
import           Import.NoFoundation
import           LN.Lib.Url          (toPrettyName)
import           LN.Lib.Validate
import           LN.T
import           Misc.Codec          (keyToInt64)
import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           All.Prelude
import           All.Profile



--
-- Handler
--

getUsersR :: Handler Value
getUsersR = run $ do
  user_id <- _requireAuthId
  (toJSON . usersToSanitizedResponses) <$> getUsersM user_id



postUserR0 :: Handler Value
postUserR0 = do
  user_id <- _requireAuthId
  user_request <- requireJsonBody :: HandlerEff UserRequest
  (toJSON . userToResponse) <$> insertUsersM user_id user_request



getUserR :: UserId -> Handler Value
getUserR lookup_user_id = do
  user_id <- _requireAuthId
  response <- getUserM user_id lookup_user_id
  if (user_id == lookup_user_id)
    then
      return $ toJSON $ userToResponse response
    else
      return $ toJSON $ userToSanitizedResponse response



getUserH :: Text -> Handler Value
getUserH _ = notFound



putUserR :: UserId -> Handler Value
putUserR lookup_user_id = do
  user_id <- _requireAuthId
  user_request <- requireJsonBody :: HandlerEff UserRequest
  (toJSON . userToResponse) <$> updateUserM user_id lookup_user_id user_request



deleteUserR :: UserId -> Handler Value
deleteUserR lookup_user_id = do

  user_id <- _requireAuthId

  void $ deleteUserM user_id lookup_user_id
  pure $ toJSON ()



getCountUsersR :: Handler Value
getCountUsersR = run $ do
  user_id <- _requireAuthId
  toJSON <$> countUsersM user_id



getUserStatsR :: Handler Value
getUserStatsR = run $ do
  user_id <- _requireAuthId
  toJSON <$> getUserStatsM user_id



getUserStatR :: UserId -> Handler Value
getUserStatR lookup_user_id = do
  user_id <- _requireAuthId
  toJSON <$> getUserStatM user_id lookup_user_id








--
-- Model/Function
--

profileNameToNick :: Text -> Text
profileNameToNick = T.toLower . T.filter isAlphaNum



userRequestToUser :: UserRequest -> User
userRequestToUser UserRequest{..} = User {
  userNick        = toPrettyName userRequestDisplayNick,
  userDisplayNick = userRequestDisplayNick,
  userName        = userRequestName,
  userEmail       = userRequestEmail,
  userEmailMD5    = "md5",
  userPlugin      = userRequestPlugin,
  userIdent       = userRequestIdent,
  userAcceptTOS   = userRequestAcceptTOS,
  userActive      = False,
  userGuard       = 0,
  userCreatedAt   = Nothing,
  userModifiedAt  = Nothing,
  userActivityAt  = Nothing
}



userToResponse :: Entity User -> UserResponse
userToResponse (Entity user_id User{..}) = UserResponse {
  userResponseId            = keyToInt64 user_id,
  userResponseNick          = userNick,
  userResponseDisplayNick   = userDisplayNick,
  userResponseName          = userName,
  userResponseEmail         = userEmail,
  userResponseEmailMD5      = userEmailMD5,
  userResponsePlugin        = userPlugin,
  userResponseIdent         = userIdent,
  userResponseAcceptTOS     = userAcceptTOS,
  userResponseActive        = userActive,
  userResponseGuard         = userGuard,
  userResponseCreatedAt     = userCreatedAt,
  userResponseModifiedAt    = userModifiedAt,
  userResponseDeactivatedAt = Nothing,
  userResponseActivityAt    = Nothing
}



usersToResponses :: [Entity User] -> UserResponses
usersToResponses users = UserResponses {
  userResponses = map userToResponse users
}



userToSanitizedResponse :: Entity User -> UserSanitizedResponse
userToSanitizedResponse (Entity user_id User{..}) = UserSanitizedResponse {
  userSanitizedResponseId          = keyToInt64 user_id,
  userSanitizedResponseNick        = userNick,
  userSanitizedResponseDisplayNick = userDisplayNick,
  userSanitizedResponseEmailMD5    = userEmailMD5,
  userSanitizedResponseActive      = userActive,
  userSanitizedResponseGuard       = userGuard,
  userSanitizedResponseCreatedAt   = userCreatedAt,
  userSanitizedResponseActivityAt  = userActivityAt
}



usersToSanitizedResponses :: [Entity User] -> UserSanitizedResponses
usersToSanitizedResponses users = UserSanitizedResponses {
  userSanitizedResponses = map userToSanitizedResponse users
}



validateUserRequest :: UserRequest -> Either Text UserRequest
validateUserRequest z@UserRequest{..} = do
--  _ <- isValidNick userRequestNick
  _ <- isValidName userRequestDisplayNick
  _ <- isValidName userRequestName
  _ <- isValidEmail userRequestEmail
  Right z








--
-- Model/Internal
--

getUsersM :: UserId -> HandlerEff [Entity User]
getUsersM user_id = do
-- selectListDb [] [] UserId

  sp@StandardParams{..} <- lookupStandardParams

  case spUserIds of

    Just user_ids -> getUsers_ByUserIdsM user_id user_ids sp
    _             -> getUsers_ByEverythingM user_id sp



getUsers_ByUserIdsM :: UserId -> [UserId] -> StandardParams -> HandlerEff [Entity User]
getUsers_ByUserIdsM _ user_ids sp = do

  selectListDb sp [UserId <-. user_ids] [] UserId



getUsers_ByEverythingM :: UserId -> StandardParams -> HandlerEff [Entity User]
getUsers_ByEverythingM _ sp = do

  selectListDb sp [] [] UserId



insertUsersM :: UserId -> UserRequest -> HandlerEff (Entity User)
insertUsersM user_id user_request = do

  -- TODO: FIXME: Fix this
  if (isSuper user_id)

    then do
      ts <- timestampH'
      let
        email_md5 = md5Text (userRequestEmail user_request)
        user = (userRequestToUser user_request) {
            userEmailMD5  = email_md5
          , userCreatedAt = Just ts
          , userActive    = True -- TODO FIXME: for now, just make all users active if they are added via this routine
        }
      new_user <- insertEntityDb user
      -- TODO FIXME: can't call this because of circular dependency issue, need to figure this out!!
      insertUsers_TasksM user_id new_user
      return new_user

    else permissionDenied "perms"



insertUsers_TasksM :: UserId -> Entity User -> HandlerEff ()
insertUsers_TasksM _ (Entity new_user_id _) = do

  -- Create a default profile
  --
  void $ insertEntityDb (profileRequestToProfile new_user_id defaultProfileRequest)

  -- Create default settings
  -- TODO
  -- void $ insertEntityDb (settingsRequestToSettings new_user_id defaultSettingsRequest)

  return ()



getUserM :: UserId -> UserId -> HandlerEff (Entity User)
getUserM user_id lookup_user_id = getUserM' user_id (UserId ==. lookup_user_id)



getUserMH :: UserId -> Text -> HandlerEff (Entity User)
getUserMH user_id lookup_user_nick = getUserM' user_id (UserNick ==. lookup_user_nick)



getUserM' :: forall t site val.
             (PersistEntity val, YesodPersist site,
              PersistQuery (YesodPersistBackend site),
              YesodPersistBackend site ~ PersistEntityBackend val) =>
             t -> Filter val -> HandlerT site IO (Entity val)
getUserM' _ q = do
  notFoundMaybe =<< selectFirstDb [q] []



updateUserM :: UserId -> UserId -> UserRequest -> HandlerEff (Entity User)
updateUserM _ lookup_user_id user_request = do

  ts <- timestampH'

  let
    email_md5 = md5Text $ (userRequestEmail user_request)
    User{..} = (userRequestToUser user_request) {
        userEmailMD5 = email_md5
      , userModifiedAt = Just ts
    }

  void $ _runDB $ updateWhere
    [ UserId ==. lookup_user_id ]

    [ UserModifiedAt =. userModifiedAt
    , UserNick        =. userNick
    , UserDisplayNick =. userDisplayNick
    , UserName        =. userName
    , UserEmail       =. userEmail
    , UserEmailMD5    =. userEmailMD5
    , UserGuard      +=. 1
    ]

  notFoundMaybe =<< selectFirstDb [ UserId ==. lookup_user_id ] []



deleteUserM :: UserId -> UserId -> HandlerEff ()
deleteUserM user_id lookup_user_id = do

  -- TODO: Fix this
  if (isSuper user_id) || (user_id == lookup_user_id)

    then
      _runDB $ delete lookup_user_id

    else
      permissionDenied "perms"




countUsersM :: UserId -> HandlerEff CountResponses
countUsersM _ = do

  StandardParams{..} <- lookupStandardParams

-- case (spOrganizationId, spUserId, spForumId, spBoardId, spThreadId) of

  case spOrganizationId of

    Just _ -> notFound

    Nothing -> do
      n <- countDb [ UserActive ==. True ]
      return $ CountResponses [CountResponse 0 (fromIntegral n)]




getUserStatsM :: UserId -> HandlerEff UserSanitizedStatResponses
getUserStatsM _ = do

  StandardParams{..} <- lookupStandardParams

  case spUserIds of

    _ -> notFound



getUserStatM :: UserId -> UserId -> HandlerEff UserSanitizedStatResponse
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
  _runDB $ do
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
