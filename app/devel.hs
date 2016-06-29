{-# LANGUAGE PackageImports #-}

module Main (
  main
) where



import           "ln-yesod" LN.Application (develMain)
import           Prelude        (IO)



main :: IO ()
main = develMain
