{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module LN.Foundation where



import           Control.Monad.Trans.Maybe            (runMaybeT)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Aeson                           (withObject)
import qualified Data.ByteString.Char8                as BSC (isPrefixOf)
import           Data.Ebyam                           (ebyam)
import qualified Data.Text                            as T (append, pack)
import qualified Data.Text                            as T (splitAt)
import qualified Data.Text.Encoding                   as T (decodeUtf8)
import           Database.Persist.Sql                 (ConnectionPool,
                                                       runSqlPool)
import qualified Database.Redis                       as R (Connection)
import qualified Network.Wai                          as W (rawPathInfo,
                                                            requestHeaders)
import           Network.Wai.Middleware.Cors          ()
import           Text.Hamlet                          (hamletFile)
import           Text.Jasmine                         (minifym)
import           Web.ServerSession.Backend.Persistent (SqlStorage (..))
import           Web.ServerSession.Frontend.Yesod     (simpleBackend)
import           Yesod.Auth.GoogleEmail2
import           Yesod.Auth.OAuth2.Github             (oauth2Github, oauth2Url)
import           Yesod.Auth.OAuth2.Github             ()
import           Yesod.Core.Types                     (Logger)
import qualified Yesod.Core.Unsafe                    as Unsafe (fakeHandlerGetLogger)
import           Yesod.Default.Util                   (addStaticContentExternal)

import           LN.Import.NoFoundation
import           LN.Misc.Codec                        (keyToInt64)
import           LN.OAuth2                            (authenticateUser)



data AppSettingsLN = AppSettingsLN {
  appRedisHost :: !String,
  appRole      :: !Role
}



data AppSettingsKeys = AppSettingsKeys {
  appGithubClientID     :: !String,
  appGithubClientSecret :: !String,
  appGoogleClientID     :: !String,
  appGoogleClientSecret :: !String
}



instance FromJSON AppSettingsLN where
  parseJSON = withObject "AppSettingsLN" $ \o -> do
    appRedisHost <- o .: "redis-host"
    appRole      <- o .: "role"
    pure AppSettingsLN {..}



instance FromJSON AppSettingsKeys where
  parseJSON = withObject "AppSettingsKeys" $ \o -> do
    appGithubClientID     <- o .: "oauth-github-client-id"
    appGithubClientSecret <- o .: "oauth-github-client-secret"
    appGoogleClientID     <- o .: "oauth-google-client-id"
    appGoogleClientSecret <- o .: "oauth-google-client-secret"
    pure AppSettingsKeys {..}



data App = App {
  appSettings        :: !AppSettings,
  appSettingsLN      :: !AppSettingsLN,
  appSettingsKeys    :: !AppSettingsKeys,
  appStatic          :: !Static, -- ^ Settings for static file serving.
                        -- Must remain lazy, otherwise we get an error in tempFoundation
  appConnPool        :: ConnectionPool, -- ^ Database connection pool.
  appHttpManager     :: !Manager,
  appLogger          :: !Logger,

  -- Custom Foundation Fields
  appGithubOAuthKeys :: !OAuthKeys,
  appGoogleOAuthKeys :: !OAuthKeys,
  appRed             :: !R.Connection,
  appZChat           :: !(TChan Text),
  appSuperUsers      :: ![Entity Super]
}



instance HasHttpManager App where
  getHttpManager = appHttpManager



mkYesodData "App" $(parseRoutesFile "config/routes")



type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)



instance Yesod App where
  approot = ApprootMaster $ appRoot . appSettings
  makeSessionBackend = simpleBackend id . SqlStorage . appConnPool
  -- makeSessionBackend _ = whenSSL sslOnlySessions $ fmap Just $ defaultClientSessionBackend
  --     sessionTimeout    -- timeout in minutes
  --     "config/client_session_key.aes"

  yesodMiddleware =
    whenSSL (sslOnlyMiddleware sessionTimeout) . defaultYesodMiddleware
    -- simpleCors . whenSSL (sslOnlyMiddleware sessionTimeout) . defaultYesodMiddleware

  defaultLayout widget = do
      master <- getYesod
      mmsg   <- getMessage
      muser  <- maybeAuth

      -- route <- getCurrentRoute
      -- let isRoute r = route == Just r
      -- We break up the default layout into two components:
      -- default-layout is the contents of the body tag, and
      -- default-layout-wrapper is the entire page. Since the final
      -- value passed to hamletToRepHtml cannot be a widget, this allows
      -- you to use normal widget features in default-layout.

      pc <- widgetToPageContent $ do
         $(widgetFile "default-layout")
      withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR

  -- Routes not requiring authentication.
  -- isAuthorized (AuthR _) _ = return Authorized
  -- isAuthorized FaviconR _ = return Authorized
  -- isAuthorized RobotsR _ = return Authorized
  -- Default to Authorized for now.
  -- isAuthorized _ _ = return Authorized

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ext mime content = do
      master <- getYesod
      let staticDir = appStaticDir $ appSettings master
      addStaticContentExternal
          minifym
          genFileName
          staticDir
          (StaticR . flip StaticRoute [])
          ext
          mime
          content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLog app _source level =
      appShouldLogAll (appSettings app)
          || level == LevelWarn
          || level == LevelError

  makeLogger = pure . appLogger

  errorHandler errorResponse = do
      $(logWarn) (T.append "Error Resp: "
                         $ T.pack (show errorResponse))
      req <- waiRequest
      let isApiRequest = BSC.isPrefixOf "/api/" $ W.rawPathInfo req
          errorText NotFound = (404, "Not Found", "Sorry, not found")
          errorText (InternalError msg) = (400, "Bad Req", msg)
          errorText (InvalidArgs m) = (400, "Bad Req", unwords m)
          errorText (PermissionDenied msg) = (403, "Forbidden", msg)
          errorText (BadMethod _) = (405, "Method not allowed",
                                          "Method not supported")
          errorText _             = (400, "Bad Request", "Boop")
      when isApiRequest $ do
          let (code, brief, full) = errorText errorResponse
          sendResponseStatus
              (mkStatus code brief)
              $ RepPlain $ toContent full
      defaultErrorHandler errorResponse



-- whenSSL :: (a -> a) -> (a -> a)
-- whenSSL f = if (appForceSSL compileTimeAppSettings) then f else id



-- | TODO FIXME - this could break ssl
--
whenSSL :: (a -> a) -> (a -> a)
whenSSL = id



-- | A session timeout of 1 week.. stop annoying me
--
sessionTimeout :: Int
sessionTimeout = 10080



instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
      master <- getYesod
      runSqlPool action $ appConnPool master



instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool



instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest _ = SPALnR -- RootR
  -- Where to send a user after logout
  logoutDest _ = SPALnR -- RootR
  -- Override the above two destinations when a Referer: header is present
  redirectToReferer _ = True

-- original:
--    authenticate creds = runDB $ do
--        x <- getBy $ UniqueUser $ credsIdent creds
--        return $ case x of
--            Just (Entity uid _) -> Authenticated uid
--            Nothing -> UserError InvalidLogin

  authenticate = runDB . authenticateUser

  -- You can add other plugins like BrowserID, email or OAuth here
  authPlugins m =
    [ oauth2Github (oauthKeysClientId $ appGithubOAuthKeys m) (oauthKeysClientSecret $ appGithubOAuthKeys m)
    , authGoogleEmail (oauthKeysClientId $ appGoogleOAuthKeys m) (oauthKeysClientSecret $ appGoogleOAuthKeys m)
    ]

  authHttpManager = getHttpManager

  loginHandler = lift $ do
    murl <- runInputGet $ iopt textField "dest"
    mapM_ setUltDest murl

    defaultLayout $ do
      setTitle "Leuro - Login"
      $(widgetFile "login")

  -- custom auth: bearer
  maybeAuthId = myMaybeAuthId



myMaybeAuthId
  :: forall master.
     (Typeable (AuthEntity master),
     YesodAuthPersist master,
     YesodPersistBackend master ~ SqlBackend)
  => HandlerT master IO (Maybe (AuthId master))
myMaybeAuthId = do

  req <- waiRequest

  -- API Requests are made by:
  -- 1. normal: setting x-api-authorization to a key found in Api[]
  -- 2. hijack: setting x-as-user to a <<key><user_id>> to become this user.
  --    API KEY MUST BE A SUPER KEY

  case (lookup "x-api-authorization" $ W.requestHeaders req, lookup "x-as-user" $ W.requestHeaders req) of
    (Just api_auth, _)      -> try_api_authorization api_auth
    (_, Just as_user_auth)  -> try_as_user_authorization as_user_auth
    _                       -> defaultMaybeAuthId

  where
  -- | Traditional API authorization via X-API-AUTHORIZATION header
  --
  try_api_authorization authHeader = do
    let lookup_api_key = T.decodeUtf8 authHeader
    m_api <- runDB $ selectFirst [ApiKey ==. lookup_api_key, ApiActive ==. True] []
    case m_api of
      Nothing                 -> permissionDenied "invalid api key"
      Just (Entity _ Api{..}) -> do
        pure $ fromPathPiece $ T.pack $ show $ keyToInt64 apiUserId

  -- | Allows us to become any user .. only super users can do this
  -- I hate this but, rightA now, it's essential for automated testing.
  -- Format: <key>:<user_id>
  --
  try_as_user_authorization authHeader = do
    let (lookup_api_key, lookup_user_id) = T.splitAt 72 $ T.decodeUtf8 authHeader
    m_super <- runMaybeT $ do
      (Entity _ Api{..}) <- MaybeT (runDB $ selectFirst [ApiKey ==. lookup_api_key, ApiActive ==. True] [])
      _                  <- MaybeT (runDB $ selectFirst [SuperUserId ==. apiUserId] [])
      pure ()
    ebyam m_super defaultMaybeAuthId $ const $ pure $ fromPathPiece lookup_user_id



instance YesodAuthPersist App



-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage



unsafeHandler :: App -> LN.Foundation.Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
