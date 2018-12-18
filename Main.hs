{-# LANGUAGE TypeApplications #-}


import System.FilePath hiding ((</>))
import System.Environment
import Exp.Print hiding (render)
import Exp.Lex
import Exp.Par
import Exp.ErrM
import Pretty
import Resolver
import Data.Monoid
import Types
import Expr
import Context
import Control.Monad
import qualified Exp.Abs as CF

loadModule :: String -> String ->  Either String (Ctx, [(String,Exp Zero)])
loadModule f s = do
  let ts = myLexer s
  case pModul ts of
      Bad err ->
        Left ("Parse failed in" <> f <> err)
      Ok (CF.Mod m) -> do
        case resolveModule m (Ctx @Zero [],[]) of
          Left err -> Left $ ("Resolver error:" <> err)
          Right ds -> return $ ds


main :: IO ()
main = do
  [arg] <- getArgs
  f <- readFile arg
  case loadModule arg f of
    Left err -> error err
    Right (Ctx cx,rs) -> do
      let r0 = mkR @Zero [] ((\(_nm,(x,e)) -> (V (Right x),Right <$> e)) <$> cx)
        --((\(nm,(x,e)) -> (V (Right x),Right <$> e)) <$> cx)
          lkNM v = case lookup v [(x,nm) | (nm,(x,_e)) <- cx] of
            Just y -> y
            Nothing -> error "lkNM: panic"
      forM_ ((applyAnyRule rs [r0])) $ \r -> putStrLn (showR lkNM r)

