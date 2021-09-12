{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
module Newtype.Union(
  module Data.Type.Coercion.Related,
  IsUnion(..),

  withUnion,
  
  unique, symmetry, greater, idemp, assocUnion
) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion.Sub (sub, equiv)
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion.Related
import Data.Type.Coercion.Related.Internal
import Data.Type.Coercion ( Coercion(Coercion))

data IsUnion x y z = IsUnion
  {
    inl :: !(Sub x z),
    inr :: !(Sub y z),
    elim :: forall r. Sub x r -> Sub y r -> Sub z r
  }

withUnion :: Related x y -> (forall xy. IsUnion x y xy -> r) -> r
withUnion (Related Coercion) body =
    body IsUnion{ inl = sub, inr = id, elim = seq }

unique :: IsUnion x y z -> IsUnion x y z' -> Coercion z z'
unique xy xy' = equiv (elim xy (inl xy') (inr xy')) (elim xy' (inl xy) (inr xy))

greater :: Sub x y -> IsUnion x y y
greater l = IsUnion{ inl = l, inr = id, elim=seq }

idemp :: IsUnion x x x
idemp = greater id

symmetry :: IsUnion x y z -> IsUnion y x z
symmetry xyz = IsUnion{ inl = inr xyz, inr = inl xyz, elim = flip (elim xyz) }

assocUnion :: IsUnion x y xy -> IsUnion xy z xy'z -> IsUnion y z yz -> IsUnion x yz x'yz -> Coercion xy'z x'yz
assocUnion xy xy'z yz x'yz =
    equiv (elim xy'z (elim xy (inl x'yz) (inr x'yz . inl yz)) (inr x'yz . inr yz))
          (elim x'yz (inl xy'z . inl xy) (elim yz (inl xy'z . inr xy) (inr xy'z)))
