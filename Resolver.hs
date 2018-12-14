{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Expr
import Types
import qualified Exp.Abs as CF


-- tail recursive form to transform a sequence of applications
-- App (App (App u v) ...) w  into (u, [v, …, w])
unApps :: CF.Exp -> [CF.Exp] -> (CF.Exp, [CF.Exp])
unApps (CF.App u v) ws = unApps u (v : ws)
unApps u         ws = (u, ws)

unVar :: CF.Exp -> Maybe CF.AIdent
unVar (CF.Var x) = Just x
unVar _       = Nothing

-- turns an expression of the form App (... (App id1 id2) ... idn)
-- into a list of idents
pseudoIdents :: CF.Exp -> Maybe [CF.AIdent]
pseudoIdents = mapM unVar . uncurry (:) . flip unApps []

pseudoTele :: [CF.PseudoTDecl] -> Maybe [(CF.AIdent, CF.Exp)]
pseudoTele []                         = return []
pseudoTele (CF.PseudoTDecl expr typ : pd) = do
    ids <- pseudoIdents expr
    pt  <- pseudoTele pd
    return $ map (,typ) ids ++ pt

binds ::  forall a. Mult -> [(CF.AIdent, CF.Exp)] -> (forall b. (String -> Either String b) -> Exp (Either String b)) -> (String -> Either String a) -> Exp (Either String a)
binds _ [] body = body
binds mult ((nm,t):bs) body = \env -> Pi (x,mult) (parse t env) (sequenceA <$> binds mult bs body (env `extend` x))
  where (CF.AIdent ((_line,_col),x)) = nm

extend :: (String -> Either String a) -> String -> String -> Either String (Next a)
extend f nm = \x -> if x == nm then Right Here else There <$> (f x)


parsePi :: Mult
        -> CF.PseudoTDecl
        -> CF.Exp
        -> (String -> Either String a)
        -> Exp (Either String a)
parsePi mult t b f = case pseudoTele [t] of
       Just tele -> binds mult tele (parse b) f
       Nothing   -> error "Telescope malformed in Pi"

parse :: forall a. CF.Exp -> (String -> Either String a) -> Exp (Either String a)
parse e0 f = case e0 of
     CF.U -> Con "Type"
     (CF.Pi t b) -> parsePi Zero t b f
     (CF.LinPi t b) -> parsePi One t b f
     (CF.App a b) -> App [parse a f,parse b f]
     (CF.Var (CF.AIdent ((_line,_col),x))) -> V (f x)
     (CF.Fun a b) -> Pi ("_",Zero) (parse a f) (There <$> parse b f)
     (CF.LFun a b) -> Pi ("_",One) (parse a f) (There <$> parse b f)




resolve :: CF.Exp -> Either String (Exp a)
resolve = sequenceA . flip parse (\nm -> Left ("variable not declared: " ++ nm))
