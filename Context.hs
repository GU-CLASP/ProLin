{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module Context where

import Unify2
import Expr
import Pretty
import Types

data Entry w = Entry w (Exp w) deriving Functor
type Context w = [Entry w] -- things available in a context.
type Rule = Exp

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs):[(y,x:ys) | (y,ys) <- select xs]

-- | Consume a variable of type @e@ from the context.
consume :: Eq w => Enumerable v => Exp (v+w) -> Avail v w -> [(Exp (v+w),PSubs v w,Avail v w)]
consume e c = do
  ((t,e'),ctx') <- select c
  case unify2 e e' of
    Just s -> return (t,s,ctx')
    Nothing -> fail "oparst"

type Metas v w = [(String,v,Exp (v+w))]
type Avail v w = [(Exp (v+w) -- term
                  ,Exp (v+w) -- type
                  )]

mkR :: forall v w. (Enumerable v) =>
       Metas v w -> -- metavariables introduced
       Avail v w -> -- context
       R w
mkR = R

data R w where
  R :: forall v w. (Enumerable v) =>
       Metas v w -> -- metavariables introduced
       Avail v w -> -- context
       R w -- each result may introduce a different number of metavariables (v)


freshMeta :: Next v + w
freshMeta = Left Here

-- | Make room for an extra metavariable
wkMeta :: (v + b) -> Next v + b
wkMeta = mapLeft There

-- | Apply a rule.

-- v is the set of free variables introduced by the matching process.
-- w are the variables of the context.
-- metaTypes: types of the meta variables.
ruleApplies :: Eq w => Enumerable v => Bool -> Exp (v+w) -> Rule (v+w) -> Metas v w -> Avail v w -> [R w]
ruleApplies consumed e (Pi (_v,Zero) dom body) metaTypes ctx =
  -- something is needed zero times. So, we create a metavariable
  ruleApplies consumed ((wkMeta <$> e) `app` V freshMeta) 
             (pushLeft <$> body)
             (("_",Here,wkMeta <$> dom) -- new meta
               :[(nm,There v,wkMeta <$> t) | (nm,v,t) <- metaTypes])
             [(wkMeta <$> w,wkMeta <$> t) | (w,t) <- ctx]
ruleApplies _consumed e (Pi (_v,One) dom body) metaTypes ctx = do
  -- something is needed one time
  (t0,PSubs _ o s,ctx') <- consume dom ctx -- see if the domain can be satisfied in the context
  let s' = \case
              Here -> V (Left Here) -- new meta variable; left alone by subst.
              There x -> wkMeta <$> case x of
                 Left y -> s y -- old meta var; substitute that
                 Right y -> V (Right y) -- regular old var; leave that
      s'' = \case
               Left x -> wkMeta <$> s x -- meta: substitute; weaken.
               Right y -> V (Right y) -- regular old var; leave that
  ruleApplies True (app e t0 >>= s'')
              (body >>= s')
              [(nm,There v,t >>= s'') | (nm,o -> There v,t) <- metaTypes]
              [(w >>= s'',t >>= s'') | (w,t) <- ctx']
ruleApplies consumed e r metaTypes ctx
  | consumed = return $ mkResult e r metaTypes ctx -- not a Pi, we have a new thing to put in the context.
  | otherwise = []

type AnyRule = Rule Zero

applyRule :: Eq w => String -> AnyRule -> R w -> [R w]
applyRule ruleName r (R metas avail) = ruleApplies False (Con ruleName) (\case <$> r) metas avail

applyAnyRule :: Eq w => [(String,AnyRule)] -> [R w] -> [R w]
applyAnyRule rs ctxs = do
  (ruleName,r) <- rs
  ctx <- ctxs
  applyRule ruleName r ctx


mkResult :: Eq w => Enumerable v => Exp (v+w) -> Exp (v+w) -> Metas v w -> Avail v w -> R w
mkResult e t metas avail = R (metas) ((e,t):avail)

prettyR :: (w -> String) ->  R w -> D
prettyR ctxNames (R m a)
  = vcat [hang 2 "metas" (vcat [text n <+> ":" <+> pretty (nm <$> e) | (n,_,e) <- m])
         ,hang 2 "lins"  (vcat [pretty (nm <$> e) <+> ":" <+> pretty (nm <$> t) | (e,t) <- a])]
     where names = [(v,n) | (n,v,_) <- m]
           nm (Right v) = ctxNames v
           nm (Left v) = case lookup v names of
             Nothing -> error "found unknown name!"
             Just x -> x
             

exampleRules :: [Exp Zero]
exampleRules =
  [foral "x" $ \x -> (Con "A" @@ x)  ⊸ (Con "B" @@ (Con "S" @@ x)) -- ∀x. A x ⊸ B (S x)
  ,foral "x" $ \x -> (Con "B" @@ x)  ⊸ (Con "A" @@ (Con "S" @@ x)) -- ∀x. B x ⊸ A (S x)
  ]


-- exampleContext :: [R]
-- exampleContext = [R @Zero @(Next Zero) [] [("a",Here,(Con "A" @@ Con "Z"))]]

showR :: (w -> String) -> R w -> String
showR f = render . prettyR f

twice :: (b -> b) -> b -> b
twice f = f . f

-- test :: [R]
-- test = twice (applyAnyRule exampleRules) exampleContext

-- >>> test
-- [metas  lins ruleApp : A (S (S Z))]

