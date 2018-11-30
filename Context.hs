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
consume :: Eq w => Enumerable v => Exp (v+w) -> Avail v w -> [(PSubs v w,Avail v w)]
consume e c = do
  ((_name,_x,e'),ctx') <- select c
  case unify2 e e' of
    Just s -> return (s,ctx')
    Nothing -> fail "oparst"

type Metas v w = [(String,v,Exp (v+w))]
type Avail v w = [(String,w,Exp (v+w))]

data R where
  R :: forall v w. (Enumerable v,Eq w) =>
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
             (("_",Here,wkMeta <$> dom) -- new meta
               :[(nm,There v,wkMeta <$> t) | (nm,v,t) <- metaTypes])
             [(nm,w,wkMeta <$> t) | (nm,w,t) <- ctx]
ruleApplies _consumed (Pi (_v,One) dom body) metaTypes ctx = do
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
  ruleApplies True
              (body >>= s')
              [(nm,There v,t >>= s'') | (nm,o -> There v,t) <- metaTypes]
              [(nm,w,t >>= s'') | (nm,w,t) <- ctx']
ruleApplies consumed r metaTypes ctx
  | consumed = return $ mkResult r metaTypes ctx -- not a Pi, we have a new thing to put in the context.
  | otherwise = []

type AnyRule = Rule Zero

applyRule :: AnyRule -> R -> [R]
applyRule r (R metas avail) = ruleApplies False (\case <$> r) metas avail

applyAnyRule :: [AnyRule] -> [R] -> [R]
applyAnyRule rs ctxs = do
  r <- rs
  ctx <- ctxs
  applyRule r ctx


mkResult :: Eq w => Enumerable v => Exp (v+w) -> Metas v w -> Avail v w -> R
mkResult e m a = R (r (id,wk) <$> m) (("ruleApp",Here,wk e):(r (There,wk) <$> a))
  where wk = (mapRight There <$>)
        r (f,g) (x,y,z) = (x,f y, g z)


prettyR :: R -> D
prettyR (R m a) =      hang 2 "metas" (hcat [text n <+> ":" <+> pretty (nm <$> e) | (n,_,e) <- m])
                </>  hang 2 "lins"  (hcat [text n <+> ":" <+> pretty (nm <$> e) | (n,_,e) <- a])
     where names = [(Left v,n) | (n,v,_) <- m] ++ [(Right w,n) | (n,w,_) <- a]
           nm v = case lookup v names of
             Nothing -> error "found unknown name!"
             Just x -> x

exampleRules :: [Exp Zero]
exampleRules =
  [foral "x" $ \x -> (Con "A" @@ x)  ⊸ (Con "B" @@ (Con "S" @@ x)) -- ∀x. A x ⊸ B (S x)
  ,foral "x" $ \x -> (Con "B" @@ x)  ⊸ (Con "A" @@ (Con "S" @@ x)) -- ∀x. B x ⊸ A (S x)
  ]


exampleContext :: [R]
exampleContext = [R @Zero @(Next Zero) [] [("a",Here,(Con "A" @@ Con "Z"))]]

instance Show R where
  show = render . prettyR

twice :: (b -> b) -> b -> b
twice f = f . f

test :: [R]
test = twice (applyAnyRule exampleRules) exampleContext

-- >>> test
-- [metas  lins ruleApp : A (S (S Z))]

