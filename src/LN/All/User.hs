{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module LN.All.User (
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
  profileNameToName,
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
  getUserMH,
  updateUserM,
  deleteUserM,
  countUsersM,
  getUserStatsM,
  getUserStatM
) where



import           Data.Char          (isAlphaNum)
import qualified Data.Text          as T (filter, toLower)
import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E

import           LN.All.Internal
import           LN.All.Prelude
import           LN.All.User.Shared (insertUsers_TasksM)



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
getUserH _ = run $ do
  errorOrJSON id $ go
  where
  go :: HandlerErrorEff ()
  go = do
    leftA Error_NotImplemented



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

profileNameToName :: Text -> Text
profileNameToName = T.toLower . T.filter isAlphaNum



userRequestToUser :: UserRequest -> User
userRequestToUser UserRequest{..} = User {
  userName            = toSafeName userRequestDisplayName,
  userDisplayName     = userRequestDisplayName,
  userFullName        = userRequestFullName,
  userEmail           = userRequestEmail,
  userEmailMD5        = "md5",
  userPlugin          = userRequestPlugin,
  userGithubIdent     = Nothing,
  userGithubCreatedAt = Nothing,
  userGoogleIdent     = Nothing,
  userGoogleCreatedAt = Nothing,
  userAcceptTOS       = userRequestAcceptTOS,
  userActive          = False,
  userGuard           = 0,
  userCreatedAt       = Nothing,
  userModifiedAt      = Nothing,
  userActivityAt      = Nothing
}



userToResponse :: Entity User -> UserResponse
userToResponse (Entity user_id User{..}) = UserResponse {
  userResponseId              = keyToInt64 user_id,
  userResponseName            = userName,
  userResponseDisplayName     = userDisplayName,
  userResponseFullName        = userFullName,
  userResponseEmail           = userEmail,
  userResponseEmailMD5        = userEmailMD5,
  userResponsePlugin          = userPlugin,
  userResponseGithubIdent     = userGithubIdent,
  userResponseGithubCreatedAt = userGithubCreatedAt,
  userResponseGoogleIdent     = userGoogleIdent,
  userResponseGoogleCreatedAt = userGoogleCreatedAt,
  userResponseAcceptTOS       = userAcceptTOS,
  userResponseActive          = userActive,
  userResponseGuard           = userGuard,
  userResponseCreatedAt       = userCreatedAt,
  userResponseModifiedAt      = userModifiedAt,
  userResponseDeactivatedAt   = Nothing,
  userResponseActivityAt      = Nothing
}



usersToResponses :: [Entity User] -> UserResponses
usersToResponses users = UserResponses {
  userResponses = map userToResponse users
}



userToSanitizedResponse :: Entity User -> UserSanitizedResponse
userToSanitizedResponse (Entity user_id User{..}) = UserSanitizedResponse {
  userSanitizedResponseId          = keyToInt64 user_id,
  userSanitizedResponseName        = userName,
  userSanitizedResponseDisplayName = userDisplayName,
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

  selectListDbE m_sp [UserId <-. user_ids, UserActive ==. True] [] UserId



getUsers_ByEverythingM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Entity User]
getUsers_ByEverythingM m_sp _ = do

  selectListDbE m_sp [UserActive ==. True] [] UserId



getUsers_ByEverything_KeysM :: Maybe StandardParams -> UserId -> HandlerErrorEff [Key User]
getUsers_ByEverything_KeysM m_sp _ = do

  selectKeysListDbE m_sp [UserActive ==. True] [] UserId



insertUsersM :: UserId -> UserRequest -> HandlerErrorEff (Entity User)
insertUsersM user_id user_request = do
  -- TODO FIXME SECURITY
  -- User Super table, loaded into appSuperUsers
  --
  is_super_user <- isSuperM user_id
  if is_super_user
    then insertUsersM' user_id user_request
    else leftA Error_PermissionDenied



insertUsersM' :: UserId -> UserRequest -> HandlerErrorEff (Entity User)
insertUsersM' _ user_request = do

  runEitherT $ do

    sanitized_user_request <- mustT $ isValidAppM $ validateUserRequest user_request
    ts                     <- lift timestampH'
    let
      email_md5 = md5Text (userRequestEmail sanitized_user_request)
      user = (userRequestToUser sanitized_user_request) {
          userEmailMD5  = email_md5
        , userCreatedAt = Just ts
        , userActive    = True -- TODO FIXME: for now, just make all users active if they are added via this routine
      }

    new_user <- mustT $ insertEntityByDbE user
    -- TODO FIXME: can't call this because of circular dependency issue, need to figure this out!!
    void $ liftIO $ insertUsers_TasksM new_user
    pure new_user




getUserMH :: UserId -> Text -> HandlerErrorEff (Entity User)
getUserMH _ lookup_user_name = do

  selectFirstDbE [UserName ==. lookup_user_name, UserActive ==. True] []



updateUserM :: UserId -> UserId -> UserRequest -> HandlerErrorEff (Entity User)
updateUserM _ lookup_user_id user_request = do

  runEitherT $ do

    sanitized_user_request <- mustT $ isValidAppM $ validateUserRequest user_request
    ts                     <- lift timestampH'

    let
      -- TODO FIXME SECURITY: Can't just let user change their email
      --
      email_md5 = md5Text $ (userRequestEmail sanitized_user_request)
      User{..} = (userRequestToUser sanitized_user_request) {
          userEmailMD5 = email_md5
        , userModifiedAt = Just ts
      }

    mustT $ updateWhereDbE
      [ UserId ==. lookup_user_id, UserActive ==. True ]

      [ UserModifiedAt =. userModifiedAt
      , UserName        =. userName
      , UserDisplayName =. userDisplayName
      , UserFullName    =. userFullName
      , UserEmail       =. userEmail
      , UserEmailMD5    =. userEmailMD5
      , UserGuard      +=. 1
      ]

    mustT $ selectFirstDbE [UserId ==. lookup_user_id] []



deleteUserM :: UserId -> UserId -> HandlerErrorEff ()
deleteUserM user_id lookup_user_id = do

  -- TODO: ACCESS: SECURITY: Fix this
  -- At least we use a Super table now
  -- Any userId found within this table, is considered a super user
  -- This table gets loaded when we start ln-yesod, into appSuperUsers within Foundation
  --
  is_super_user <- isSuperM user_id
  if is_super_user || user_id == lookup_user_id

    then
      deleteDbE lookup_user_id

    else
      leftA Error_PermissionDenied




countUsersM :: Maybe StandardParams -> UserId -> HandlerErrorEff CountResponses
countUsersM m_sp _ = do

  case (lookupSpMay m_sp spOrganizationId) of

    Just _ -> leftA Error_NotImplemented

    _      -> do
      n <- countDb [UserActive ==. True]
      rightA $ CountResponses [CountResponse 0 (fromIntegral n)]




getUserStatsM :: Maybe StandardParams -> UserId -> HandlerErrorEff UserSanitizedStatResponses
getUserStatsM _ _ = leftA Error_NotImplemented



getUserStatM :: UserId -> UserId -> HandlerErrorEff UserSanitizedStatResponse
getUserStatM _ lookup_user_id = do

  (a,b,c,d) <- qUserStats lookup_user_id

  let (threads,thread_posts,resources,leurons) = (E.unValue a, E.unValue b, E.unValue c, E.unValue d)

  rightA $ UserSanitizedStatResponse {
    userSanitizedStatResponseUserId      = keyToInt64 lookup_user_id,
    userSanitizedStatResponseThreads     = threads,
    userSanitizedStatResponseThreadPosts = thread_posts,
    userSanitizedStatResponseRespect     = 0,
    userSanitizedStatResponseResources   = resources,
    userSanitizedStatResponseLeurons     = leurons,
    userSanitizedStatResponseWorkouts    = 0
  }



qUserStats
  :: forall site.  (YesodPersist site, YesodPersistBackend site ~ SqlBackend)
  => Key User
  -> ControlMA (HandlerT site IO) (E.Value Int64, E.Value Int64, E.Value Int64, E.Value Int64)
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
