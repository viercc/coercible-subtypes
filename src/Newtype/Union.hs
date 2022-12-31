{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Type.Coercion ( Coercion(Coercion), TestCoercion(..))

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

instance Eq (IsUnion x y z) where
  _ == _ = True
instance Ord (IsUnion x y z) where
  compare _ _ = EQ

instance TestCoercion (IsUnion x y) where
  testCoercion u1 u2 = Just (unique u1 u2)

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
--   Note: combining @idemp@ and 'unique', @IsUnion x x z -> Coercion x z@ holds.
idemp :: IsUnion x x x
idemp = greater id

-- | Union is commutative.
--
--   Note: combining @commutative@ and 'unique', @IsUnion x y xy -> IsUnion y x yx -> Coercion xy yx@ holds.
commutative :: IsUnion x y z -> IsUnion y x z
commutative xyz = IsUnion{ inl = inr xyz, inr = inl xyz, elim = flip (elim xyz) }

-- | Union is associative.
--
--   Note: combining @associative@ and 'unique', the following holds.
--   
--   >    IsUnion x y xy -> IsUnion xy z xy'z
--   > -> IsUnion y z yz -> IsUnion x yz x'yz
--   > -> Coercion xy'z x'yz 
associative :: IsUnion x y xy -> IsUnion xy z xyz -> IsUnion y z yz -> IsUnion x yz xyz
associative xy xy'z yz =
    IsUnion {
       inl = inl xy'z . inl xy 
     , inr = elim yz (inl xy'z . inr xy) (inr xy'z)
     , elim = \x_r yz_r -> elim xy'z (elim xy x_r (yz_r . inl yz)) (yz_r . inr yz)
    }
