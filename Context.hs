{-# LANGUAGE TypeOperators #-}


module Context where

import Unify2
import Expr

type Entry w = (w,Exp w)
type Context w = [Entry w]
type Rule = Exp

anyUnify :: Eq w => Enumerable v => Exp (v+w) -> Context w -> [PSubs v w]
anyUnify e c = do
  (x,e') <- c
  case unify2 e e' of
    Just s -> return s
    Nothing -> fail "oparst"


-- v is the set of free variables introduced by the matching process.
-- w are the variables of the context.
ruleApplies :: Rule (v+w) -> Context w -> [Exp w]
ruleApplies r [] = [r]
ruleApplies (Pi v dom body) ctx = do
  s <- anyUnify dom ctx -- see if the domain can be satisfied in the context
  applySubst s (pushRight <$> body) $ \body' -> ruleApplies body' ctx
