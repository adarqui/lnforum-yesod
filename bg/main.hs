module Main (
  main
) where



import           LN.Control.Monad
import           LN.Job



main :: IO ()
main = do
  putStrLn "ln-bg"
  runJobs "config/workers/dev.yaml"
  forever $ getLine
