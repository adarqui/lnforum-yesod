module Main (
  main
) where



import           Control.Monad
import           LN.Job



main :: IO ()
main = do
  putStrLn "ln-bg"
  runJobs "config/workers/dev.yaml"
  forever $ getLine
