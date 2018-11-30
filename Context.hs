{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}


module Context where

import Unify2
import Expr

data Entry w = Entry w (Exp w) deriving Functor
type Context w = [Entry w] -- things available in a context.
type Rule = Exp

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs):[(y,x:ys) | (y,ys) <- select xs]

-- | Consume a variable of type @e@ from the context.
consume :: Eq w => Enumerable v => Exp (v+w) -> Avail v w -> [(PSubs v w,Avail v w)]
consume e c = do
  ((_x,e'),ctx') <- select c
  case unify2 e e' of
    Just s -> return (s,ctx')
    Nothing -> fail "oparst"

type Metas v w = [(v,Exp (v+w))]
type Avail v w = [(w,Exp (v+w))]

data R where
  R :: (Enumerable v,Eq w) =>
       Metas v w -> -- metavariables introduced
       Avail v w -> -- context
       R -- each result may introduce a different number of metavariables (v)


-- | Make room for an extra metavariable
wkMeta :: (v + b) -> Next v + b
wkMeta = mapLeft There

-- | Apply a rule.

-- v is the set of free variables introduced by the matching process.
-- w are the variables of the context.
-- metaTypes: types of the meta variables.
ruleApplies :: Eq w => Enumerable v => Bool -> Rule (v+w) -> Metas v w -> Avail v w -> [R]
ruleApplies consumed (Pi (_v,Zero) dom body) metaTypes ctx =
  -- something is needed zero times. So, we create a metavariable
  ruleApplies consumed
             (pushLeft <$> body)
             ((Here,wkMeta <$> dom) -- new meta
               :[(There v,wkMeta <$> t) | (v,t) <- metaTypes])
             [(w,wkMeta <$> t) | (w,t) <- ctx]
ruleApplies consumed (Pi (_v,One) dom body) metaTypes ctx = do
  -- something is needed one time
  (PSubs _ o s,ctx') <- consume dom ctx -- see if the domain can be satisfied in the context
  let s' = \case
              Here -> V (Left Here) -- new meta variable; left alone by subst.
              There x -> wkMeta <$> case x of
                 Left y -> s y -- old meta var; substitute that
                 Right y -> V (Right y) -- regular old var; leave that
      s'' = \case
               Left x -> wkMeta <$> s x -- meta: substitute; weaken.
               Right y -> V (Right y) -- regular old var; leave that
  ruleApplies consumed
              (body >>= s')
              [(There v,t >>= s'') | (o -> There v,t) <- metaTypes]
              [(w,t >>= s'') | (w,t) <- ctx']
ruleApplies consumed r metaTypes ctx
  | consumed = return $ mkResult r metaTypes ctx -- not a Pi, we have a new thing to put in the context.
  | otherwise = []


mkResult :: Eq w => Enumerable v => Exp (v+w) -> Metas v w -> Avail v w -> R
mkResult e m a = R m ((_,e):a)