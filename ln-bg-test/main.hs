module Main (
  main
) where



import           LN.Generate   (defaultProfileRequest)
import           LN.Job.Enqueue
import           LN.Misc.Codec (int64ToKey')
import           LN.T



main :: IO ()
main = do
  putStrLn "bg-test"
  mkJob_CreateUserProfile (int64ToKey' 1) defaultProfileRequest
  putStrLn "done."
