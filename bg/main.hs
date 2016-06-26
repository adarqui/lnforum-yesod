module Main where



import           Control.Monad
import           Job



main :: IO ()
main = do
  putStrLn "ln-bg"
  runJobs "config/workers/dev.yaml"
  forever $ getLine
