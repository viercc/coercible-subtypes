{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
module Newtype.Union(
  module Data.Type.Coercion.Related,
  IsUnion(..),

  withUnion,
  
  unique, greater, idemp, commutative, associative
) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion.Sub (sub, equiv)
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion.Related
import Data.Type.Coercion.Related.Internal
import Data.Type.Coercion ( Coercion(Coercion))

-- | @IsUnion x y z@ witnesses the fact:
--
--   * All @x, y, z@ share the same runtime representation
--   * @z@ is a union type of @x@ and @y@. In other words, the following three holds:
--
--     * @'Sub' x z@
--     * @Sub y z@
--     * For any type @r@ satisfying both of @(Sub x r, Sub y r)@, @Sub z r@.
data IsUnion x y z = IsUnion
  {
    inl :: !(Sub x z), -- ^ @x@ can be safely coerced to @z@
    inr :: !(Sub y z), -- ^ @y@ can be safely coerced to @z@
    elim :: forall r. Sub x r -> Sub y r -> Sub z r
      -- ^ Given both @x@ and @y@ can be safely coerced to @r@, too @z@ can.
  }

-- | For a pair of 'Related' types @x@ and @y@, make some (existentially quantified)
--   type @xy@ where @xy@ is a union type of @x, y@.
withUnion :: Related x y -> (forall xy. IsUnion x y xy -> r) -> r
withUnion (Related Coercion) body =
    body IsUnion{ inl = sub, inr = id, elim = seq }

-- | Two union types @z,z'@ of the same pair of types @x,y@ may be different,
--   but they are equivalent in terms of coercibility.
unique :: IsUnion x y z -> IsUnion x y z' -> Coercion z z'
unique xy xy' = equiv (elim xy (inl xy') (inr xy')) (elim xy' (inl xy) (inr xy))

-- | When @Sub x y@, @y@ itself is a union type of @x, y@.
greater :: Sub x y -> IsUnion x y y
greater l = IsUnion{ inl = l, inr = id, elim=seq }

-- | Union is idempotent.
--
--   Note: combining @idemp@ and 'unique', @IsUnion x x z -> Coercible x z@ holds.
idemp :: IsUnion x x x
idemp = greater id

-- | Union is commutative.
--
--   Note: combining @commutative@ and 'unique', @IsUnion x y xy -> IsUnion y x yx -> Coercible xy yx@ holds.
commutative :: IsUnion x y z -> IsUnion y x z
commutative xyz = IsUnion{ inl = inr xyz, inr = inl xyz, elim = flip (elim xyz) }

-- | Union is associative.
associative :: IsUnion x y xy -> IsUnion xy z xy'z -> IsUnion y z yz -> IsUnion x yz x'yz -> Coercion xy'z x'yz
associative xy xy'z yz x'yz =
    equiv (elim xy'z (elim xy (inl x'yz) (inr x'yz . inl yz)) (inr x'yz . inr yz))
          (elim x'yz (inl xy'z . inl xy) (elim yz (inl xy'z . inr xy) (inr xy'z)))
