{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module TopLevel where

import Exp.Par
import Exp.ErrM
import Resolver
import Data.Monoid
import Types
import Expr
import Context
import qualified Exp.Abs as CF

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

-- | run fuel lookupName initialState rules
run :: Int -> R -> [(String, AnyRule)] -> IO ()
run 0  _r _rs = putStrLn "DONE (no fuel)"
run n  r rs = do
  putStrLn "-------------------"
  putStrLn "State:"
  print r
  case applyAnyRule rs [r] of
    [] -> putStrLn "No rule applies."
    (r':_) -> run (n-1)  r' rs

