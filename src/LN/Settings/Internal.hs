module LN.Settings.Internal (
  configSettingsDevYml,
  configSettingsPrivateDevYml,
  configSettingsProductionYml,
  configSettingsPrivateProductionYml
) where



import           Data.Monoid ((<>))
import           System.IO   (FilePath)



settingsBasePublic :: FilePath
settingsBasePublic = "config/settings"



settingsBasePrivate :: FilePath
settingsBasePrivate = "private/settings"



-- | Location of the default config file.
configSettingsDevYml :: FilePath
configSettingsDevYml = settingsBasePublic <> "/dev.yml"



configSettingsPrivateDevYml :: FilePath
configSettingsPrivateDevYml = settingsBasePrivate <> "/dev.yml"



configSettingsProductionYml :: FilePath
configSettingsProductionYml = settingsBasePublic <> "/production.yml"



configSettingsPrivateProductionYml :: FilePath
configSettingsPrivateProductionYml = settingsBasePrivate <> "/production.yml"
