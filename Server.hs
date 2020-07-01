-- module Server where

import Network.Simple.TCP
import TransportationParser
import Network.Socket
import System.IO
import TopLevel
import Context
import HaskToProlin
import Expr
import Data.Maybe (maybe)
import Options

extractOutput :: R -> (Maybe String,R)
extractOutput r =
  case pullOutputFromContext r of
    Just (r',outMsg) -> (Just (show outMsg),r')
    Nothing -> (Nothing,r)

clientLoop :: Handle -> R -> [(String, AnyRule)] -> IO ()
clientLoop h r rs = do
   putStrLn "WAITING FOR CLIENT..."
   x <- hGetLine h
   putStr ("Client says: " ++ show x ++ "\n")
   let m = parseTop x
   case m of
     Left _ -> hPutStrLn h "I did not quite get that ..."
     Right p -> do
       putStrLn ("Got: " ++ show p)
       let p' = encode p
       putStrLn ("Encoded as: " ++ show p')
       -- Push p in the context so the manager can deal with it.
       r' <- run defaults (pushInContext (Symb "p",Symb "Message" `app` (p')) r) rs
       let (reply,r'') = extractOutput r'
       hPutStrLn h (maybe "Tell me more..." id reply)
       clientLoop h r'' rs

startServer :: IO ()
startServer =  do
  putStrLn ("starting server on port " ++ port)
  serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
    (r,rs) <- loadAndPrepareModule "transport.pli"
    h <- socketToHandle connectionSocket ReadWriteMode
    clientLoop h r rs
  where port = "8000"

main :: IO ()
main = startServer
