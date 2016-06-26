module Main where



import           Job
import           LN.T
import           Misc.Codec (int64ToKey')



main :: IO ()
main = do
  putStrLn "bg-test"
  mkJob_CreateUserProfile (int64ToKey' 1) defaultProfileRequest
  putStrLn "done."
