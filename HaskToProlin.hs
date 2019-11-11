{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HaskToProlin where

import GHC.Generics
import GHC.TypeLits
import Expr
import Data.Proxy

instance Encode String where
  encode s = Con s

instance Encode Int where
  encode s = Con (show s) -- ugh

class Encode a where
  encode :: a -> Exp ()
  default encode :: (Generic a, Encode' (Rep a)) => a -> Exp ()
  encode x = encode' (from x)


class Encode' f where
  encode' :: f p -> Exp ()

class EncodeMany f where
  encodeMany :: f p -> [Exp ()]

instance Encode' x => Encode' (D1 meta x) where
  encode' (M1 x) = encode' x
instance (Encode' f, Encode' g) => Encode' (f :+: g) where
  encode' (L1 x) = encode' x
  encode' (R1 x) = encode' x

instance (KnownSymbol consName,EncodeMany f) => Encode' (C1 ('MetaCons consName 'PrefixI isRecord) f) where
  encode' (M1 x) = App (Con (symbolVal (Proxy @consName)):encodeMany x)

instance (EncodeMany f, EncodeMany g) => EncodeMany (f :*: g) where
  encodeMany  (l :*: r) = encodeMany l ++ encodeMany r

instance EncodeMany x => EncodeMany (S1 meta x) where
  encodeMany (M1 x) = encodeMany x
instance (Encode c) => EncodeMany (K1 i c) where
  encodeMany (K1 x) = [encode x]
instance EncodeMany U1 where
  encodeMany _ = []

instance Encode a => Encode (Maybe a)
instance (Encode a, Encode b) => Encode (Either a b)
