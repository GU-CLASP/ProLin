{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}


module Context where

import Unify2
import Expr

data Entry w = Entry w (Exp w) deriving Functor
type Context w = [Entry w]
type Rule = Exp

anyUnify :: Eq w => Enumerable v => Exp (v+w) -> Context w -> [PSubs v w]
anyUnify e c = do
  (Entry _x e') <- c
  case unify2 e e' of
    Just s -> return s
    Nothing -> fail "oparst"

data R w where
  R :: Enumerable v => Exp (v+w) -> R w

-- v is the set of free variables introduced by the matching process.
-- w are the variables of the context.
ruleApplies :: Eq w => Enumerable v => Rule (v+w) -> Context (v+w) -> Context w -> [R w]
ruleApplies (Pi (_v,Zero) _dom body) metaTypes ctx =
  ruleApplies (pushLeft <$> body) (fmap (fmap (mapLeft There)) metaTypes) ctx
ruleApplies (Pi (_v,One) dom body) metaTypes ctx = do
  ss <- anyUnify dom ctx -- see if the domain can be satisfied in the context
  case ss of
    PSubs _ s -> let s' = \case
                       Here -> V (Left Here) -- new meta variable; left alone by subst.
                       There x -> fmap (mapLeft There) $ case x of
                          Left y -> s y -- old meta var; map that
                          Right y -> V (Right y) -- regular old var; leave that
              in ruleApplies (body >>= s') (fmap (error "oops:metas") metaTypes) ctx 
ruleApplies r _ctx metaTypes = [R r]
-- metaTypes: filter out the metas that were substituted out; apply s' in those that were not.

