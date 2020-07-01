import TopLevel
import Options

main :: IO ()
main = do
  opts <- readOptions
  (r0,rs) <- loadAndPrepareModule (optProgramFile opts)
  _ <- run opts r0 rs
  return ()
  where

