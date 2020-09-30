import TopLevel
import Options
import Server

main :: IO ()
main = do
  opts <- readOptions
  (r0,rs) <- loadAndPrepareModule (optProgramFile opts)
  if optServer opts
    then startServer opts r0 rs
    else run opts r0 rs
  return ()

