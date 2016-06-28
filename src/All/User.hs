{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module All.User (
  -- Handler
  getUsersR,
  postUserR0,
  getUserR,
  getUserH,
  putUserR,
  deleteUserR,
  getUsersCountR,
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
  getUsers_ByEverything_KeysM,
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
  sp      <- lookupStandardParams
  errorOrJSON usersToSanitizedResponses $ getUsersM (pure sp) user_id



postUserR0 :: Handler Value
postUserR0 = run $ do
  user_id      <- _requireAuthId
  user_request <- requireJsonBody :: HandlerEff UserRequest
  errorOrJSON userToResponse $ insertUsersM user_id user_request



getUserR :: UserId -> Handler Value
getUserR lookup_user_id = run $ do
  user_id <- _requireAuthId
  e_user <- getUserM user_id lookup_user_id
  if (user_id == lookup_user_id)
    then errorOrJSON userToResponse (pure e_user)
    else errorOrJSON userToSanitizedResponse (pure e_user)



getUserH :: Text -> Handler Value
getUserH _ = pure $ toJSON Error_NotImplemented



putUserR :: UserId -> Handler Value
putUserR lookup_user_id = run $ do
  user_id       <- _requireAuthId
  user_request <- requireJsonBody :: HandlerEff UserRequest
  errorOrJSON userToResponse $ updateUserM user_id lookup_user_id user_request



deleteUserR :: UserId -> Handler Value
deleteUserR lookup_user_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ deleteUserM user_id lookup_user_id



getUsersCountR :: Handler Value
getUsersCountR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ countUsersM (pure sp) user_id



getUserStatsR :: Handler Value
getUserStatsR = run $ do
  user_id <- _requireAuthId
  sp      <- lookupStandardParams
  errorOrJSON id $ getUserStatsM (pure sp) user_id



getUserStatR :: UserId -> Handler Value
getUserStatR lookup_user_id = run $ do
  user_id <- _requireAuthId
  errorOrJSON id $ getUserStatM user_id lookup_user_id








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

getUsersM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity User]
getUsersM m_sp user_id = do

  case (lookupSpMay m_sp spUserIds) of

    Just user_ids -> getUsers_ByUserIdsM m_sp user_id user_ids
    _             -> getUsers_ByEverythingM m_sp user_id



getUsers_ByUserIdsM :: Maybe StandardParams -> UserId -> [UserId] -> HandlerErrorEff [Entity User]
getUsers_ByUserIdsM m_sp _ user_ids = do

  selectListDbEither m_sp [UserId <-. user_ids, UserActive ==. True] [] UserId



getUsers_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity User]
getUsers_ByEverythingM m_sp _ = do

  selectListDbEither m_sp [UserActive ==. True] [] UserId



getUsers_ByEverything_KeysM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Key User]
getUsers_ByEverything_KeysM m_sp _ = do

  selectKeysListDbEither m_sp [] [] UserId



insertUsersM :: UserId -> UserRequest -> HandlerErrorEff (Entity User)
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
      void $ insertUsers_TasksM user_id new_user
      right $ new_user

    else left Error_PermissionDenied



insertUsers_TasksM :: UserId -> Entity User -> HandlerErrorEff ()
insertUsers_TasksM _ (Entity new_user_id _) = do

  -- Create a default profile
  --
  void $ insertEntityDb (profileRequestToProfile new_user_id defaultProfileRequest)

  -- Create default settings
  -- TODO
  -- void $ insertEntityDb (settingsRequestToSettings new_user_id defaultSettingsRequest)

  right ()



getUserM :: UserId -> UserId -> HandlerErrorEff (Entity User)
getUserM _ lookup_user_id = do

  selectFirstDbEither [UserId ==. lookup_user_id, UserActive ==. True] []



getUserMH :: UserId -> Text -> HandlerErrorEff (Entity User)
getUserMH _ lookup_user_nick = do

  selectFirstDbEither [UserNick ==. lookup_user_nick, UserActive ==. True] []




updateUserM :: UserId -> UserId -> UserRequest -> HandlerErrorEff (Entity User)
updateUserM _ lookup_user_id user_request = do

  ts <- timestampH'

  let
    email_md5 = md5Text $ (userRequestEmail user_request)
    User{..} = (userRequestToUser user_request) {
        userEmailMD5 = email_md5
      , userModifiedAt = Just ts
    }

  void $ updateWhereDb
    [ UserId ==. lookup_user_id, UserActive ==. True ]

    [ UserModifiedAt =. userModifiedAt
    , UserNick        =. userNick
    , UserDisplayNick =. userDisplayNick
    , UserName        =. userName
    , UserEmail       =. userEmail
    , UserEmailMD5    =. userEmailMD5
    , UserGuard      +=. 1
    ]

  selectFirstDbEither [UserId ==. lookup_user_id] []



deleteUserM :: UserId -> UserId -> HandlerErrorEff ()
deleteUserM user_id lookup_user_id = do

  -- TODO: ACCESS: SECURITY: Fix this
  if (isSuper user_id) || (user_id == lookup_user_id)

    then
      deleteDbEither lookup_user_id

    else
      left Error_PermissionDenied




countUsersM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countUsersM m_sp _ = do

  case (lookupSpMay m_sp spOrganizationId) of

    Just _ -> left Error_NotImplemented

    _ -> do
      n <- countDb [UserActive ==. True]
      right $ CountResponses [CountResponse 0 (fromIntegral n)]




getUserStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff UserSanitizedStatResponses
getUserStatsM _ _ = left Error_NotImplemented



getUserStatM :: UserId -> UserId -> HandlerErrorEff UserSanitizedStatResponse
getUserStatM _ lookup_user_id = do

  (a,b,c,d) <- qUserStats lookup_user_id

  let (threads,thread_posts,resources,leurons) = (E.unValue a, E.unValue b, E.unValue c, E.unValue d)

  right $ UserSanitizedStatResponse {
    userSanitizedStatResponseUserId      = keyToInt64 lookup_user_id,
    userSanitizedStatResponseThreads     = threads,
    userSanitizedStatResponseThreadPosts = thread_posts,
    userSanitizedStatResponseRespect     = 0,
    userSanitizedStatResponseResources   = resources,
    userSanitizedStatResponseLeurons     = leurons,
    userSanitizedStatResponseWorkouts    = 0
  }



qUserStats :: forall site.
  (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
  Key User ->
  ControlMA (HandlerT site IO) (E.Value Int64, E.Value Int64, E.Value Int64, E.Value Int64)
qUserStats user_id = do
  _runDB $ do
    (leurons:[]) <- E.select
      $ E.from $ \leuron -> do
      E.where_ $ leuron ^. LeuronUserId E.==. E.val user_id
      pure (E.countDistinct $ leuron ^. LeuronId)

    (resources:[]) <- E.select
      $ E.from $ \resource -> do
      E.where_ $ resource ^. ResourceUserId E.==. E.val user_id
      pure (E.countDistinct $ resource ^. ResourceId)

    (thread_posts:[]) <- E.select
      $ E.from $ \thread_post -> do
      E.where_ $ thread_post ^. ThreadPostUserId E.==. E.val user_id
      pure (E.countDistinct $ thread_post ^. ThreadPostId)

    (threads:[]) <- E.select
      $ E.from $ \thread -> do
      E.where_ $ thread ^. ThreadUserId E.==. E.val user_id
      pure (E.countDistinct $ thread ^. ThreadId)

    pure (threads :: E.Value Int64, thread_posts :: E.Value Int64, resources :: E.Value Int64, leurons :: E.Value Int64)
