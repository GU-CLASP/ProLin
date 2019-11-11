
import System.Environment
import TopLevel

main :: IO ()
main = do
  [arg] <- getArgs
  (r0,rs) <- loadAndPrepareModule arg
  c <- run 10 r0 rs
  case c of
    False -> putStrLn "No more fuel."
    True -> putStrLn "No rule applies."
  return ()

