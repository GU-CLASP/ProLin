{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Expr
import Types

resolve' :: forall a. (String -> Either String a) -> Exp String -> Exp (Either String a)
resolve' f = \case
     Pi (nm,mult) dom bod -> case sequenceA bod of
       Here -> error "resolve': malformed input"
       There bod' -> Pi (nm,mult) (resolve' f dom) (sequenceA <$> resolve' (extend nm) bod')
     App xs -> App (map (resolve' f) xs)
     Con x -> Con x
     V x -> V (f x)
  where
    extend :: String -> String -> Either String (Next a)
    extend nm x = if x == nm then Right Here else There <$> (f x)

resolve :: Exp String -> Either String (Exp a)
resolve = sequenceA . resolve' (\nm -> Left ("variable not declared: " ++ nm))
