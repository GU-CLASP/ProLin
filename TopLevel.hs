{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module TopLevel where

import Exp.Par
import Exp.ErrM
import Resolver
import Types
import Expr
import Context
import qualified Exp.Abs as CF
import Control.Monad (when)
import Options

-- | loadModule fileName fileContents -> Either error (context,[(ruleName,ruleExpression)])
loadModule :: String -> String ->  Either String (Ctx, [(String,AnyRule)])
loadModule f s = do
  let ts = myLexer s
  case pModul ts of
      Bad err ->
        Left ("Parse failed in" <> f <> err)
      Ok (CF.Mod m) -> do
        case resolveModule m (Ctx @Zero [],[]) of
          Left err -> Left $ ("Resolver error:" <> err)
          Right ds -> return $ ds

-- | Give a name to all the rules and construct the R structure
prepareContext :: Ctx -> R
prepareContext (Ctx cx) = R @Zero lkNM [] ((\(_nm,(x,e)) -> (V (Right x),Right <$> e)) <$> cx)
   where
          lkNM v = case lookup v [(x,nm) | (nm,(x,_e)) <- cx] of
            Just y -> y
            Nothing -> error "lkNM: panic"

loadAndPrepareModule :: FilePath -> IO (R, [(String, AnyRule)])
loadAndPrepareModule fname = do
  f <- readFile fname
  return $ case loadModule fname f of
    Left err -> error err
    Right (cx,rs) -> (prepareContext cx,rs)

run :: Options -> R -> [(String, AnyRule)] -> IO R
run Options {..} = go optFuel
 where
     -- | run fuel initialState lookupName rules
   go :: Int -> R -> [(String, AnyRule)] -> IO R
   go 0  r _rs = do
     putStrLn "No more fuel"
     return r
   go n  r rs = do
     putStrLn "-------------------"
     putStrLn "State:"
     print r
     case applyAnyRule rs [r] of
       [] -> do
         putStrLn "No more rules to apply"
         return r
       ((ruleName,r'):_) -> do
         when (optPauseStep || optPauseInteractive && (haveConstructor "Utter" r || haveConstructor "Heard" r)) $ do
           putStrLn "Press <ENTER> to continue"
           _ <- getLine
           return ()
         putStrLn ("Applied: " ++ ruleName)
         go (n-1) r' rs
