module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Auth.OAuth2.Github
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2.Github ()

import Model.User (authenticateUser)
import qualified Network.Wai as W
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text as T
import qualified Database.Redis as R

import qualified Data.ByteString.Char8 as BSC

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    -- custom
    , appGithubOAuthKeys   :: OAuthKeys
--    , appGoogleOAuthKeys :: OAuthKeys
    , appRed               :: R.Connection
    , appZChat             :: TChan Text
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = whenSSL sslOnlySessions $ fmap Just $ defaultClientSessionBackend
        sessionTimeout    -- timeout in minutes
        "config/client_session_key.aes"

--    yesodMiddleware = (sslOnlyMiddleware 120) . defaultYesodMiddleware
    yesodMiddleware =
      whenSSL (sslOnlyMiddleware sessionTimeout) . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuth
        -- route <- getCurrentRoute

        -- let isRoute r = route == Just r

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
          addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
          addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js"
          addStylesheet $ StaticR css_bootstrap_css
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

    makeLogger = return . appLogger

    errorHandler errorResponse = do
        $(logWarn) (T.append "Error Resp: "
                           $ T.pack (show errorResponse))
        req <- waiRequest
        let isApiRequest = BSC.isPrefixOf "/api/" $ W.rawPathInfo req
            errorText NotFound = (404, "Not Found", "Sorry, not found")
            errorText (InternalError msg) = (400, "Bad Req", msg)
            errorText (InvalidArgs m) = (400, "Bad Req", unwords m)
            errorText (PermissionDenied msg) = (403, "Forbidden", msg)
            errorText (BadMethod _) = (405, "Method Not Allowed",
                                            "Method not supported")
            errorText _             = (400, "Bad Request", "Boop")
        when isApiRequest $ do
            let (code, brief, full) = errorText errorResponse
            sendResponseStatus
                (mkStatus code brief)
                $ RepPlain $ toContent $ T.append "Error: " full
        defaultErrorHandler errorResponse


whenSSL :: (a -> a) -> (a -> a)
whenSSL f = if (appForceSSL compileTimeAppSettings) then f else id

-- | A session timeout of 1 week.. stop annoying me
--
sessionTimeout :: Int
sessionTimeout = 10080

-- How to run database actions.
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
    loginDest _ = SPAR -- RootR
    -- Where to send a user after logout
    logoutDest _ = SPAR -- RootR
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
    authPlugins m = addAuthBackDoor m
      [
        authBrowserId def,
        oauth2Github (oauthKeysClientId $ appGithubOAuthKeys m) (oauthKeysClientSecret $ appGithubOAuthKeys m)
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


myMaybeAuthId :: (YesodAuthPersist master, Typeable (AuthEntity master))
  => HandlerT master IO (Maybe (AuthId master))
myMaybeAuthId = do
  req <- waiRequest
-- DEBUG:  liftIO $ print req
  case lookup "z-authorization" (W.requestHeaders req) of
    Nothing -> defaultMaybeAuthId
    Just authHeader -> do
-- DEBUG:      liftIO $ print authHeader
      return $ fromPathPiece $ T.decodeUtf8 authHeader

-- custom
addAuthBackDoor :: App -> [AuthPlugin App] -> [AuthPlugin App]
addAuthBackDoor app =
    if appAllowDummyAuth (appSettings app) then (authDummy :) else id

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- getsYesod :: MonadHandler m => m (HandlerSite m)
-- getsYesod = undefined
--

-- requireAuthId = do
--  (Entity user_id _) <- requireAuth
--  return user_id

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
