{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LN.Application (
  getApplicationDev,
  appMain,
  develMain,
  makeFoundation,
  -- * for DevelMain
  getApplicationRepl,
  shutdownApp,
  -- * for GHCI
  handler,
  db
) where



import           Control.Monad.Logger                 (liftLoc, runLoggingT)
import qualified Data.Text                            as T (pack, unpack)
import           Database.Persist.Postgresql          (createPostgresqlPool,
                                                       pgConnStr, pgPoolSize,
                                                       runSqlPool)
import qualified Database.Redis                       as R
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings, defaultShouldDisplayException,
                                                       getPort, runSettings,
                                                       setHost, setOnException,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)

import           LN.All.Api
import           LN.All.Bucket
import           LN.All.BucketResource
import           LN.All.BucketLeuron
import           LN.All.Leuron
import           LN.All.LeuronTraining
import           LN.All.Me
import           LN.All.Pack.Bucket
import           LN.All.Pack.Leuron
import           LN.All.Pack.Me
import           LN.All.Pack.Resource
import           LN.All.Pack.Sanitized.User
import           LN.All.Pack.User
import           LN.All.Profile
import           LN.All.Resource
import           LN.All.SPA
import           LN.All.User
import           LN.All.Templates
import           LN.Handler.Common
import           LN.Import

import qualified Data.Proxy                           as P
import qualified Web.ServerSession.Backend.Persistent as SS
import qualified Web.ServerSession.Core               as SS



mkMigrate "migrateAll" (SS.serverSessionDefs (P.Proxy :: P.Proxy SS.SessionMap) ++ entityDefs)



-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp



-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> AppSettingsLN -> AppSettingsKeys -> IO App
makeFoundation appSettings appSettingsLN appSettingsKeys = do

--    dbconf <- if appDatabaseUrl appSettings
  appRed <- R.connect (R.defaultConnectInfo { R.connectHost = appRedisHost appSettingsLN, R.connectPort = R.PortNumber 6379 })

  -- example chat initialization
  appZChat <- atomically newBroadcastTChan

  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
      (if appMutableStatic appSettings then staticDevel else static)
      (appStaticDir appSettings)

  -- custom
  appGithubOAuthKeys <- pure $ OAuthKeys (T.pack $ appGithubClientID appSettingsKeys) (T.pack $ appGithubClientSecret appSettingsKeys)
  appGoogleOAuthKeys <- pure $ OAuthKeys (T.pack $ appGoogleClientID appSettingsKeys) (T.pack $ appGoogleClientSecret appSettingsKeys)

  let appSuperUsers = []

  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool = App {..}
      -- The App {..} syntax is an example of record wild cards. For more
      -- information, see:
      -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger

  -- Create the database connection pool
  pool <- flip runLoggingT logFunc $ createPostgresqlPool
      (pgConnStr  $ appDatabaseConf appSettings)
      (pgPoolSize $ appDatabaseConf appSettings)

  -- Perform database migration using our application's logging settings.
  if appRole appSettingsLN == RoleWeb
    then runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    else runLoggingT (runSqlPool (pure ()) pool) logFunc

  -- Patchwork
  let app = mkFoundation pool
  (app_super_users :: [Entity Super]) <- db' app (selectList [] [])

  -- Return the foundation
  pure $ app {
    appSuperUsers = app_super_users
  }



-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- mkRequestLogger $ def {
    outputFormat =
      if appDetailedRequestLogging $ appSettings foundation
         then Detailed True
         else Apache
              (if appIpFromHeader $ appSettings foundation
                  then FromFallback
                  else FromSocket)
    , destination = Logger $ loggerSet $ appLogger foundation
  }

  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  pure $ logWare $ defaultMiddlewaresNoLogging appPlain



-- | just prints bleh.. can be added like so: return $ bleh $ logWare $ ...
-- bleh :: Middleware
-- bleh app req sendResponse = do
--  app req $ \rsp -> do
--      putStrLn "bleh!"
--      sendResponse rsp



-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation)
  $ setHost (appHost $ appSettings foundation)
  $ setOnException (\_req e ->
      when (defaultShouldDisplayException e) $ messageLoggerSource
          foundation
          (appLogger foundation)
          $(qLocation >>= liftLoc)
          "yesod"
          LevelError
          (toLogStr $ "Exception from Warp: " <> show e))
    defaultSettings



-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings    <- getAppSettings
  ln_settings <- getAppSettingsLN
  app_keys    <- getAppSettingsKeys
  foundation  <- makeFoundation settings ln_settings app_keys
  wsettings   <- getDevSettings $ warpSettings foundation
  app         <- makeApplication foundation
  pure (wsettings, app)



getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettingsArgv [] useEnv



getAppSettingsLN :: IO AppSettingsLN
getAppSettingsLN = loadYamlSettingsArgv [] useEnv



getAppSettingsKeys :: IO AppSettingsKeys
getAppSettingsKeys = loadYamlSettingsArgv [] useEnv



-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev



-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do

  args <- (map T.unpack) <$> getArgs

  -- Get the settings from all relevant sources
  settings <- loadYamlSettings
    -- fall back to compile-time values, set to [] to require values at runtime
    args
    []
    -- [configSettingsYmlValue]
    -- allow environment variables to override
    useEnv

  ln_settings <- loadYamlSettings
    args
    []
    -- [configSettingsYmlValue]
    useEnv

  app_keys <- loadYamlSettings
    args
    []
    -- [configSettingsYmlValue]
    useEnv

  -- Generate the foundation from the settings
  foundation <- makeFoundation settings ln_settings app_keys

  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation

  -- Run the application with Warp
  -- simpleCors so we can make cross domain ajax requests
  -- runSettings (warpSettings foundation) $ simpleCors app
  runSettings (warpSettings foundation) $ app



--------------------------------------------------------------
-- Function for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings    <- getAppSettings
  ln_settings <- getAppSettingsLN
  app_keys    <- getAppSettingsKeys
  foundation  <- makeFoundation settings ln_settings app_keys
  wsettings   <- getDevSettings $ warpSettings foundation
  app1        <- makeApplication foundation
  pure (getPort wsettings, foundation, app1)



shutdownApp :: App -> IO ()
shutdownApp _ = pure ()


---------------------------------------------
-- Function for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = do
  settings    <- getAppSettings
  ln_settings <- getAppSettingsLN
  app_keys    <- getAppSettingsKeys
  makeFoundation settings ln_settings app_keys >>= flip unsafeHandler h



-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB



-- | Custom handler for pre-built App
--
-- Created initially for user with appSuperUsers
--
handler' :: App -> Handler a -> IO a
handler' app h = do
  unsafeHandler app h

-- | Custom db query runner for pre-built App
--
-- Created initially for user with appSuperUsers
--
db' :: App -> ReaderT SqlBackend (HandlerT App IO) a -> IO a
db' app = handler' app . runDB
