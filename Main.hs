
import System.Environment
import TopLevel

main :: IO ()
main = do
  [arg] <- getArgs
  (r0,rs) <- loadAndPrepareModule arg
  _ <- run 10 r0 rs
  return ()

