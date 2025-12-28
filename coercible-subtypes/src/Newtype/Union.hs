{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Newtype.Union(
  module Data.Type.Coercion.Related,

  -- * Union types
  IsUnion(..),
  withUnion,
  
  -- * Properties of union types
  unique, greater, idemp, commutative, associative, distributive
) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion.Sub (sub, equiv)
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion.Related
import Data.Type.Coercion.Related.Internal
import Data.Type.Coercion ( Coercion(Coercion), TestCoercion(..))

-- | @'IsUnion' x y z@ witnesses that @z@ is a union type of @x, y@.
--
--   Concretely, a value of @IsUnion x y z@ is a proof for the following three conditions.
--
--   * @'Sub' x z@
--   * @Sub y z@
--   * For any type @r@ satisfying both of @(Sub x r, Sub y r)@, @Sub z r@.
--   
--   If we could write /the/ union type of @x@ and @y@ as @x ∨ y@,
--   @IsUnion x y z@ means @x ∨ y ≈ z@, where @≈@ means @'Coercion'@ relation.
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

-- | For a pair of 'Related' types @x@ and @y@, there exists a union type @xy@.
withUnion :: Related x y -> (forall xy. IsUnion x y xy -> r) -> r
withUnion (Related Coercion) body =
    body IsUnion{ inl = sub, inr = id, elim = seq }

-- | Two union types @z, z'@ of the same pair of types @x,y@ may be different,
--   but they are equivalent in terms of coercibility.
--
--   Using hypothetical @x ∨ y@ notation, this function states:
--
--   > unique :: (x ∨ y ≈ z) -> (x ∨ y ≈ z') -> (z ≈ z')
unique :: IsUnion x y z -> IsUnion x y z' -> Coercion z z'
unique xy xy' = equiv (elim xy (inl xy') (inr xy')) (elim xy' (inl xy) (inr xy))

-- | When @Sub x y@, @y@ itself is a union type of @x, y@.
--
-- Using hypothetical @x ∨ y@ notation, this function states:
--
-- > greater :: Sub x y -> (x ∨ y ≈ y)
greater :: Sub x y -> IsUnion x y y
greater l = IsUnion{ inl = l, inr = id, elim=seq }

-- | Union is idempotent.
--
-- Using hypothetical @x ∨ y@ notation, this function states:
--
-- > idemp :: x ∨ x ≈ x
idemp :: IsUnion x x x
idemp = greater id

-- | Union is commutative.
--
-- Using hypothetical @x ∨ y@ notation, this function states:
--
-- > commutative :: (x ∨ y ≈ z) -> (y ∨ x ≈ z)
commutative :: IsUnion x y z -> IsUnion y x z
commutative xyz = IsUnion{ inl = inr xyz, inr = inl xyz, elim = flip (elim xyz) }

-- | Union is associative.
--
-- Using hypothetical @x ∨ y@ notation, this function states:
--
-- > associative
-- >  :: (x ∨ y ≈ xy) -> (xy ∨ z ≈ xyz) -> (y ∨ z ≈ yz)
-- >  -> (x ∨ yz ≈ xyz)
--   
-- Or more simply:
--
-- > associative :: (x ∨ y) ∨ z ≈ x ∨ (y ∨ z)
associative :: IsUnion x y xy -> IsUnion xy z xyz -> IsUnion y z yz -> IsUnion x yz xyz
associative xy xy'z yz =
    IsUnion {
       inl = inl xy'z . inl xy 
     , inr = elim yz (inl xy'z . inr xy) (inr xy'z)
     , elim = \x_r yz_r -> elim xy'z (elim xy x_r (yz_r . inl yz)) (yz_r . inr yz)
    }

-- | Union is distributive.
-- 
--   Using hypothetical @x ∨ y@ notation, this function states:
--
--   > distributive
--   >  :: Sub x (a ∨ b) -> exists a' b'. (Sub a' a, Sub b' b, a' ∨ b' ≈ x)
--
--   In words and figures, if there exists an @x@ smaller than @c ≈ a ∨ b@,
--
--   >      c ≈ a ∨ b
--   >     /|\
--   >    / | \
--   >   /  |  \
--   >  a   |   b
--   >      |
--   >      x
--
--   there exists @a', b'@ such that @x ≈ a' ∨ b'@.
--
--   >      c ≈ a ∨ b
--   >     /|\
--   >    / | \
--   >   /  |  \
--   >  a   |   b
--   >  |   |   |
--   >  |   x ≈ a' ∨ b'
--   >  |  / \  |
--   >  | /   \ |
--   >  |/     \|
--   >  a'      b'
--
--   Combined with the properties of "Newtype.Intersection",
--   'distributive' is equivalent to the usual notion of distributivity below.
--
--   > (x ∨ y) ∧ z ≈ (x ∧ z) ∨ (y ∧ z)
--
--   See 'Newtype.LatticeProperties.distIU' for more.
distributive
  :: forall a b c x.
     IsUnion a b c
  -> Sub x c
  -> forall r. (forall a' b'. Sub a' a -> Sub b' b -> IsUnion a' b' x -> r)
  -> r
distributive abc xc body = case (ac, bc, xc) of
  (Sub Coercion, Sub Coercion, Sub Coercion)
    -> body @x @x sub sub IsUnion{ inl = sub, inr = sub, elim = seq }
  where
    ac = inl abc
    bc = inr abc

-- NB: Unlike the other properties, @distributive@ is not possible to
--  implement using only @inl, inr, elim@ and
--  it is /necessary/ to use the internal @Coercion@ of the @Sub@.