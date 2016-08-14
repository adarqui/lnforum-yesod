module LN.Settings.Internal (
    settingsBasePublic
  , settingsBasePrivate
) where



import           System.IO   (FilePath)



settingsBasePublic :: FilePath
settingsBasePublic = "config/settings"



settingsBasePrivate :: FilePath
settingsBasePrivate = "private/settings"
