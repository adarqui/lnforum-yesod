module Main (
  main
) where



import           LN.Job
import           LN.Misc.Codec (int64ToKey')
import           LN.T



main :: IO ()
main = do
  putStrLn "bg-test"
  mkJob_CreateUserProfile (int64ToKey' 1) defaultProfileRequest
  putStrLn "done."
