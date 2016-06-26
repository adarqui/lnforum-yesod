module Main where



import           Job
import           LN.T



main :: IO ()
main = do
  putStrLn "bg"
  enq_CreateUserProfile 1 defaultProfileRequest
  putStrLn "done."
