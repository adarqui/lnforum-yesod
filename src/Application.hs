{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (
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



import qualified Data.Text as T (pack)
import           Control.Monad.Logger                 (liftLoc, runLoggingT)
import           Database.Persist.Postgresql          (createPostgresqlPool,
                                                       pgConnStr, pgPoolSize,
                                                       runSqlPool)
import           Import
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
import qualified Database.Redis                       as R



import           All.Api
import           All.Board
import           All.Forum
import           All.GlobalGroup
import           All.Leuron
import           All.Like
import           All.Me
import           All.Organization
import           All.Pack.Board
import           All.Pack.Forum
import           All.Pack.GlobalGroup
import           All.Pack.Leuron
import           All.Pack.Me
import           All.Pack.Organization
import           All.Pack.PmIn
import           All.Pack.PmOut
import           All.Pack.Resource
import           All.Pack.Sanitized.User
import           All.Pack.Team
import           All.Pack.TeamMember
import           All.Pack.Thread
import           All.Pack.ThreadPost
import           All.Pm
import           All.PmIn
import           All.PmOut
import           All.Profile
import           All.Reminder
import           All.Resource
import           All.SPA
import           All.Star
import           All.Team
import           All.TeamMember
import           All.Thread
import           All.ThreadPost
import           All.User

import           Handler.Common


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp



-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> AppSettingsLN -> IO App
makeFoundation appSettings appSettingsLN = do

--    dbconf <- if appDatabaseUrl appSettings
    appRed <- R.connect (R.defaultConnectInfo { R.connectHost = appRedisHost appSettingsLN, R.connectPort = R.PortNumber 16379 })

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
    appGithubOAuthKeys <- pure $ OAuthKeys (T.pack $ appGithubClientID appSettingsLN) (T.pack $ appGithubClientSecret appSettingsLN)

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
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool



-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
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
    return $ logWare $ defaultMiddlewaresNoLogging appPlain



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
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings



-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings    <- getAppSettings
    ln_settings <- getAppSettingsLN
    foundation  <- makeFoundation settings ln_settings
    wsettings   <- getDevSettings $ warpSettings foundation
    app         <- makeApplication foundation
    return (wsettings, app)



getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv



getAppSettingsLN :: IO AppSettingsLN
getAppSettingsLN = loadYamlSettings [configSettingsYml] [] useEnv



-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev



-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    ln_settings <- loadYamlSettingsArgs
        [configSettingsYmlValue]
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings ln_settings

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
    foundation  <- makeFoundation settings ln_settings
    wsettings   <- getDevSettings $ warpSettings foundation
    app1        <- makeApplication foundation
    return (getPort wsettings, foundation, app1)



shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Function for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = do
  settings    <- getAppSettings
  ln_settings <- getAppSettingsLN
  makeFoundation settings ln_settings >>= flip unsafeHandler h



-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
