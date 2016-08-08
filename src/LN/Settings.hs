{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.

module LN.Settings (
  DB,
  OAuthKeys (..),
  AppSettings (..),
  Environment (..),
  widgetFileSettings,
  combineSettings,
  widgetFile,
  configSettingsYmlBS,
  configSettingsYmlValue,
  compileTimeAppSettings,
  combineStylesheets,
  combineScripts
) where



import           ClassyPrelude.Yesod
import           Control.Exception           (throw)
import           Data.Aeson                  (Result (..), fromJSON, withObject,
                                              withText, (.!=), (.:?))
import           Data.FileEmbed              (embedFile)
import           Data.Yaml                   (decodeEither')
import           Database.Persist.Postgresql (PostgresConf)
import           Language.Haskell.TH.Syntax  (Exp, Name, Q)
import           LN.Settings.Internal        (configSettingsDevYml)
import           Network.Wai.Handler.Warp    (HostPreference)
import           Yesod.Default.Config2       (applyEnvValue)
import           Yesod.Default.Util          (WidgetFileSettings,
                                              widgetFileNoReload,
                                              widgetFileReload)


--
-- CUSTOM
--

type DB a = forall (m :: * -> *).
  (MonadIO m, Functor m) => ReaderT SqlBackend m a



data OAuthKeys = OAuthKeys {
  oauthKeysClientId :: Text,
  oauthKeysClientSecret :: Text
}



data Environment
  = EnvProduction
  | EnvStaging
  | EnvDevelopment
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Environment where
  parseJSON = withText "Environment" $ \t -> do
    case t of
      "production"  -> pure EnvProduction
      "staging"     -> pure EnvStaging
      "development" -> pure EnvDevelopment
      _             -> fail "Failed to parse environment"

instance ToJSON Environment where
  toJSON env = case env of
    EnvProduction  -> "production"
    EnvStaging     -> "staging"
    EnvDevelopment -> "development"

--
-- END CUSTOM
--



-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
  { appStaticDir              :: String
  -- ^ Directory from which to serve static files.
  , appDatabaseConf           :: PostgresConf
  -- ^ Configuration settings for accessing the database.
  , appRoot                   :: Text
  -- ^ Base for all generated URLs.
  , appHost                   :: HostPreference
  -- ^ Host/interface the server should bind to.
  , appPort                   :: Int
  -- ^ Port to listen on
  , appIpFromHeader           :: Bool
  -- ^ Get the IP address from the header when logging. Useful when sitting
  -- behind a reverse proxy.

  , appDetailedRequestLogging :: Bool
  -- ^ Use detailed request logging system
  , appShouldLogAll           :: Bool
  -- ^ Should all log messages be displayed?
  , appReloadTemplates        :: Bool
  -- ^ Use the reload version of templates
  , appMutableStatic          :: Bool
  -- ^ Assume that files in the static dir may change after compilation
  , appSkipCombining          :: Bool
  -- ^ Perform no stylesheet/script combining

  -- Example app-specific configuration values.
  , appCopyrightA             :: Text
  -- ^ CopyrightA text to appear in the footer of the page
  , appAnalytics              :: Maybe Text
  -- ^ Google Analytics code

  -- custom
  , appAllowDummyAuth         :: Bool
  , appForceSSL               :: Bool
  , appEnvironment            :: Environment
  }



instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    let defaultDev =
#if DEVELOPMENT
          True
#else
          False
#endif
    appStaticDir              <- o .: "static-dir"
    appDatabaseConf           <- o .: "database"
    appRoot                   <- o .: "approot"
    appHost                   <- fromString <$> o .: "host"
    appPort                   <- o .: "port"
    appIpFromHeader           <- o .: "ip-from-header"

    appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
    appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
    appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
    appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
    appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

    appCopyrightA             <- o .: "copyright"
    appAnalytics              <- o .:? "analytics"

    -- custom
    appAllowDummyAuth         <- o .:? "allow-dummy-auth" .!= defaultDev
    appForceSSL               <- o .:? "force-ssl"        .!= (not defaultDev)
    appEnvironment            <- o .: "env"

    pure AppSettings {..}



-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def



-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def



-- The rest of this file contains settings which rarely need changing by a
-- user.
--
widgetFile :: String -> Q Exp
widgetFile s =
 if appReloadTemplates compileTimeAppSettings
    then widgetFileReload widgetFileSettings s
    else widgetFileNoReload widgetFileSettings s



-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsDevYml)



-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS



-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
      Error e -> error e
      Success settings -> settings



-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
  (appSkipCombining compileTimeAppSettings)
  combineSettings



combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
  (appSkipCombining compileTimeAppSettings)
  combineSettings
