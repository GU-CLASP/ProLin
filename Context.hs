{-# LANGUAGE PartialTypeSignatures #-}
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
import Data.Monoid
import Data.Either (isRight)
import Control.Monad (guard)

data Entry w = Entry w (Exp w) deriving Functor
type Context w = [Entry w] -- things available in a context.
type Rule = Exp

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs):[(y,x:ys) | (y,ys) <- select xs]

-- | Consume a variable of type @e@ from the context.
-- Return a triple with: Term, Substitution, Rest of the context.
consume :: Eq w => Enumerable v => Exp (v+w) -> Avail v w -> [((Exp (v+w),Exp (v+w)),PSubs v w,Avail v w)]
consume e c = do
  ((t,e'),ctx') <- select c
  case unify2 e e' of
    Just s -> return ((t,e'),s,ctx')
    Nothing -> fail "oparst"

type Metas v w = [(String,v,Exp (v+w))]
type Avail v w = [(Exp (v+w) -- term
                  ,Exp (v+w) -- type
                  )]

-- mkR :: forall v w. (Enumerable v) =>
--        Metas v w -> -- metavariables introduced
--        Avail v w -> -- context
--        R w
-- mkR = R

data R  where
  R :: forall v w. (Enumerable v, Eq w) =>
       (w -> String) -> -- names of context variables
       Metas v w -> -- metavariables introduced
       Avail v w -> -- context
       R  -- each result may introduce a different number of metavariables (v)


freshMeta :: Next v + w
freshMeta = Left Here

-- | Make room for an extra metavariable
wkMeta :: (v + b) -> Next v + b
wkMeta = mapLeft There

wkCtx :: (v + b) -> v + Next b
wkCtx = mapRight There


-- | Apply a rule.

findFreshName :: Int -> String -> [String] -> String
findFreshName n x xs | candidate `elem` xs = findFreshName (n+1) x xs
                     | otherwise = candidate
  where candidate = x ++ show n

isGround :: Exp (v+w) -> Bool
isGround = getAll . foldMap (All . isRight)

-- v is the set of free variables introduced by the matching process.
-- w are the variables of the context.
-- metaTypes: types of the meta variables.
ruleApplies :: Eq w => Enumerable v => Show v
  => (w -> String) -- | user-friendly names for w
  -> Bool -- | was there anything consumed at all?
  -> Exp (v+w) -- | constructed expression
  -> Rule (v+w) -- | rule considered
  -> Metas v w -- | metas
  -> Avail v w -- | context
  -> [R]
ruleApplies wN consumed e (Pi (vNm,Zero) dom body) metaTypes ctx =
  -- something is needed zero times. So, we create a metavariable
  ruleApplies wN consumed ((wkMeta <$> e) `app` V freshMeta) 
             (pushLeft <$> body)
             ((findFreshName 0 vNm metaNames,Here,wkMeta <$> dom) -- new meta
               :[(nm,There v,wkMeta <$> t) | (nm,v,t) <- metaTypes])
             [(wkMeta <$> w,wkMeta <$> t) | (w,t) <- ctx]
  where metaNames = [nm | (nm,_,_) <- metaTypes]
ruleApplies wN _consumed e (Pi (_v,One keep unicity) dom body) metaTypes ctx = do
  let solutions = consume dom ctx -- see if the domain can be satisfied in the context
  -- something is needed one time
  ((t0,ty0),PSubs _ o s,ctx') <- solutions
  case unicity of
    AnyUnicity -> return ()
    Unique -> guard (length solutions == 1)
  -- it does: we need to substitute the consumed thing
  let s' = \case
              Here -> t0 >>= s'' -- the variable bound by Pi (unknown to unifier). Substituted by the context element.
              There x -> s'' x
      s'' = \case
               Left x -> s x -- meta: substitute according to unifier
               Right y -> V (Right y) -- regular old var; leave it alone
  ruleApplies wN True (app e t0 >>= s'')
              (body >>= s')
              [(nm,v,t >>= s'') | (nm,o -> There v,t) <- metaTypes]
              ([(t0 >>= s'', ty0 >>= s'') | keep == Release] ++
               [(w >>= s'',t >>= s'') | (w,t) <- ctx'] )
ruleApplies wN True e (Rec fs) metaTypes ctx = return $ applyRec wN e fs metaTypes ctx -- a record: put all the components in the context
ruleApplies wN consumed e r metaTypes ctx
  | consumed = return $ R wN (metaTypes) ((e,r):ctx)   -- not a Pi, we have a new thing to put in the context.
  | otherwise = []

-- | Put all the components of the telescope in the context.
-- FIXME: projections!
applyRec :: Eq w => Enumerable v
         => (w -> String)
         -> Exp (v + w)
         -> Tele (v + w)
         -> Metas v w
         -> Avail v w
         -> R
applyRec w _ TNil metaTypes ctx = R w metaTypes ctx
applyRec w e (TCons (x,Zero) f fs) metaTypes ctx = error "ZERO"
applyRec w e (TCons (x,One _ _) f fs) metaTypes ctx
  = applyRec w' (wkCtx <$> e)
               (pushRight <$> fs)
               [(nm,v,wkCtx <$> t) | (nm,v,t) <- metaTypes] (both (wkCtx <$>) <$>((e,f):ctx))
    where w' (Here) = x
          w' (There y) = w y

type AnyRule = Rule Zero

applyRule :: String -> AnyRule -> R -> [R]
applyRule ruleName r (R wN metas avail) = ruleApplies wN False (Symb ruleName) (\case <$> r) metas avail

applyAnyRule :: [(String,AnyRule)] -> [R] -> [R]
applyAnyRule rs ctxs = do
  (ruleName,r) <- rs
  ctx <- ctxs
  applyRule ruleName r ctx


prettyR :: R -> D
prettyR  (R ctxNames m a)
  = vcat [hang 2 "metas" (vcat [text n <+> ":" <+> pretty (nm <$> e) | (n,_,e) <- m])
         ,hang 2 "lins"  (vcat [pretty (nm <$> e) <+> ":" <+> pretty (nm <$> t) | (e,t) <- a])]
     where names = [(v,n) | (n,v,_) <- m]
           nm (Right v) = ctxNames v
           nm (Left v) = case lookup v names of
             Nothing -> error ("found unknown name for meta:" ++ show v)
             Just x -> x

exampleRules :: [Exp Zero]
exampleRules =
  [foral "x" $ \x -> (Symb "A" @@ x)  ⊸ (Symb "B" @@ (Symb "S" @@ x)) -- ∀x. A x ⊸ B (S x)
  ,foral "x" $ \x -> (Symb "B" @@ x)  ⊸ (Symb "A" @@ (Symb "S" @@ x)) -- ∀x. B x ⊸ A (S x)
  ]

-- exampleContext :: [R]
-- exampleContext = [R @Zero @(Next Zero) [] [("a",Here,(Con "A" @@ Con "Z"))]]

instance Show R where
  show = render . prettyR

twice :: (b -> b) -> b -> b
twice f = f . f

-- test :: [R]
-- test = twice (applyAnyRule exampleRules) exampleContext

-- >>> test
-- [metas  lins ruleApp : A (S (S Z))]

pushInContext :: (Exp Zero,Exp Zero) -> R -> R
pushInContext x (R wN metas ctx) = R wN metas (both (exNihilo <$>) x:ctx)


pullOutputFromContext :: R -> Maybe (R,Exp Zero)
pullOutputFromContext r = case applyRule "pull" pullRule r of
  [] -> Nothing
  (R _ _ []:_) -> error "pullOutputFromContext: panic: could pull but nothing remains in the context"
  (R wN metas ((_pullTerm,msg):avails):_) -> case isClosed msg of
    Nothing -> error "manager attempted to output a message with free variables."
    Just msg' -> Just (R wN metas avails,msg')
  where pullRule = foral "m" $ \msg -> (Symb "Output" @@ msg) ⊸ msg
