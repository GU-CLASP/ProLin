-- module Server where

import Network.Simple.TCP
import TransportationParser
import Network.Socket
import System.IO
import TopLevel
import Context
import HaskToProlin
import Expr

managerLoop :: R -> [(String, AnyRule)] -> IO (String, R)
managerLoop r rs = do
  putStrLn "-------------------"
  putStrLn "State:"
  print r
  case pullOutputFromContext r of
    Just (r',outMsg) -> return (show outMsg,r')
    Nothing -> case applyAnyRule rs [r] of
      [] -> return ("Tell me more!",r) -- nothing to do any more
      (r':_) -> managerLoop r' rs


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
       (reply,r') <- managerLoop (pushInContext (Symb "p",Symb "Message" `app` (p')) r) rs
       hPutStrLn h reply
       clientLoop h r' rs

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
