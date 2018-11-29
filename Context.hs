{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}


module Context where

import Unify2
import Expr

data Entry w = Entry w Mult (Exp w) deriving Functor
type Context w = [Entry w] -- things available in a context.
type Rule = Exp

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs):[(y,x:ys) | (y,ys) <- select xs]

-- | Consume a variable of type @e@ from the context.
consume :: Eq w => Enumerable v => Exp (v+w) -> Context w -> [(PSubs v w,Context w)]
consume e c = do
  (Entry _x One e',ctx') <- select c
  case unify2 e e' of
    Just s -> return (s,ctx')
    Nothing -> fail "oparst"

type Metas v w = [(v,Exp (v+w))]

data R w where
  R :: Enumerable v =>
       Exp (v+w) -> -- thing added
       Metas v w -> -- metavariables introduced
       Context w -> -- context
       R w -- each result may introduce a different number of metavariables (v)


-- v is the set of free variables introduced by the matching process.
-- w are the variables of the context.
-- metaTypes: types of the meta variables.

-- | Make room for an extra metavariable
wkMeta :: (v + b) -> Next v + b
wkMeta = mapLeft There

-- | Apply a rule.
ruleApplies :: Eq w => Enumerable v => Rule (v+w) -> Metas v w -> Context w -> [R w]
ruleApplies (Pi (_v,Zero) dom body) metaTypes ctx =
  -- something is needed zero times. So, we create a metavariable
  ruleApplies (pushLeft <$> body) ((Here,wkMeta <$> dom) -- new meta
                                    :[(There v,wkMeta <$> t) | (v,t) <- metaTypes]) ctx
ruleApplies (Pi (_v,One) dom body) metaTypes ctx = do
  (ss,ctx') <- consume dom ctx -- see if the domain can be satisfied in the context
  case ss of
    PSubs _ o s ->
      let s' = \case
                  Here -> V (Left Here) -- new meta variable; left alone by subst.
                  There x -> wkMeta <$> case x of
                     Left y -> s y -- old meta var; map that
                     Right y -> V (Right y) -- regular old var; leave that
          s'' = \case
                   Left x -> wkMeta <$> s x -- meta: substitute; weaken.
                   Right y -> V (Right y) -- regular old var; leave that
      in ruleApplies (body >>= s') [(There v,t >>= s'') | (o -> There v,t) <- metaTypes] ctx'
ruleApplies r metaTypes ctx = [(R r metaTypes ctx)] -- not a Pi, we have a new thing to put in the context.

