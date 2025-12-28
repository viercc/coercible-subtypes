{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Newtype.Intersection(
  module Data.Type.Coercion.Related,

  -- * Intersection types
  IsIntersection(..),
  withIntersection,
  
  -- * Properties of intersection types
  unique, lesser, idemp, commutative, associative, distributive
) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion.Sub (equiv, sub)
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion.Related
import Data.Type.Coercion.Related.Internal
import Data.Type.Coercion ( Coercion(Coercion), TestCoercion(..) )

-- | @'IsIntersection' x y z@ witnesses that @z@ is an intersection type of @x, y@.
--
--   Concretely, a value of @IsIntersection x y z@ is a proof for the following three conditions.
--
--   * @'Sub' z x@
--   * @Sub z y@
--   * For any type @s@ satisfying both of @(Sub s x, Sub s y)@, @Sub s z@.
--   
--   If we could write /the/ intersection type of @x@ and @y@ as @x ∧ y@,
--   @IsIntersection x y z@ means @x ∧ y ≈ z@, where @≈@ means @'Coercion'@ relation.
data IsIntersection x y z = IsIntersection
  {
    proj1 :: !(Sub z x),
    proj2 :: !(Sub z y),
    conjunct :: forall s. Sub s x -> Sub s y -> Sub s z
  }

instance Eq (IsIntersection x y z) where
  _ == _ = True
instance Ord (IsIntersection x y z) where
  compare _ _ = EQ

instance TestCoercion (IsIntersection x y) where
  testCoercion u1 u2 = Just (unique u1 u2)

-- | For a pair of 'Related' types @x@ and @y@, there exists an intersection type @xy@.
withIntersection :: Related x y -> (forall xy. IsIntersection x y xy -> r) -> r
withIntersection (Related Coercion) body =
    body IsIntersection{ proj1 = sub, proj2 = id, conjunct = seq }

-- | Two intersection types @z, z'@ of the same pair of types @x,y@ may be different,
--   but they are equivalent in terms of coercibility.
--
--   Using hypothetical @x ∧ y@ notation, this function states:
--
--   > unique :: (x ∧ y ≈ z) -> (x ∧ y ≈ z') -> (z ≈ z')
unique :: IsIntersection x y z -> IsIntersection x y z' -> Coercion z z'
unique xy xy' = equiv (conjunct xy' (proj1 xy) (proj2 xy)) (conjunct xy (proj1 xy') (proj2 xy'))

-- | When @Sub x y@, @x@ itself is an intersection type of @x, y@.
--
--   Using hypothetical @x ∧ y@ notation, this function states:
--
--   > lesser :: Sub x y -> (x ∧ y ≈ x)
lesser :: Sub x y -> IsIntersection x y x
lesser l = IsIntersection{ proj1=id, proj2=l, conjunct= \sx !_ -> sx }


-- | Intersection is idempotent.
--
-- Using hypothetical @x ∧ y@ notation, this function states:
--
-- > idemp :: x ∧ x ≈ x
idemp :: IsIntersection x x x
idemp = lesser id

-- | Intersection is commutative.
--
-- Using hypothetical @x ∧ y@ notation, this function states:
--
-- > commutative :: (x ∧ y ≈ z) -> (y ∧ x ≈ z)
commutative :: IsIntersection x y z -> IsIntersection y x z
commutative xyz = IsIntersection{ proj1 = proj2 xyz, proj2 = proj1 xyz, conjunct = flip (conjunct xyz)}

-- | Intersection is associative.
--
-- Using hypothetical @x ∧ y@ notation, this function states:
--
-- > associative
-- >  :: (x ∧ y ≈ xy) -> (xy ∧ z ≈ xyz) -> (y ∧ z ≈ yz)
-- >  -> (x ∧ yz ≈ xyz)
--   
-- Or more simply:
--
-- > associative :: (x ∧ y) ∧ z ≈ x ∧ (y ∧ z)
associative :: IsIntersection x y xy -> IsIntersection xy z xyz -> IsIntersection y z yz -> IsIntersection x yz xyz
associative xy xy'z yz =
    IsIntersection {
       proj1 = proj1 xy . proj1 xy'z
     , proj2 = conjunct yz (proj2 xy . proj1 xy'z) (proj2 xy'z)
     , conjunct = \s_x s_yz -> conjunct xy'z (conjunct xy s_x (proj1 yz . s_yz)) (proj2 yz . s_yz)
    }

-- | Intersection is distributive.
-- 
--   Using hypothetical @x ∧ y@ notation, this function states:
--
--   > distributive
--   >  :: Sub (a ∧ b) x -> exists a' b'. (Sub a a', Sub b b', a' ∧ b' ≈ x)
--
--   In words and figures, if there exists an @x@ larger than @c ≈ a ∧ b@,
--
--   >      x
--   >      |
--   >  a   |   b
--   >   \  |  /
--   >    \ | /
--   >     \|/
--   >      c ≈ a ∧ b
--
--   there exists @a', b'@ such that @x ≈ a' ∧ b'@.
--
--   >      
--   >  a'      b'
--   >  |\     /|
--   >  | \   / |
--   >  |  \ /  |
--   >  |   x ≈ a' ∧ b'
--   >  |   |   |
--   >  a   |   b
--   >   \  |  /
--   >    \ | /
--   >     \|/
--   >      c ≈ a ∧ b
--
--   Combined with the properties of "Newtype.Union",
--   'distributive' is equivalent to the usual notion of distributivity below.
--
--   > (x ∧ y) ∨ z ≈ (x ∨ z) ∧ (y ∨ z)
--
--   See also 'Newtype.LatticeProperties.distUI'.
distributive
  :: forall a b c x.
     IsIntersection a b c
  -> Sub c x
  -> forall r. (forall a' b'. Sub a a' -> Sub b b' -> IsIntersection a' b' x -> r)
  -> r
distributive abc cx body = case (ca, cb, cx) of
  (Sub Coercion, Sub Coercion, Sub Coercion)
    -> body @x @x sub sub IsIntersection{ proj1 = sub, proj2 = sub, conjunct = seq }
  where
    ca = proj1 abc
    cb = proj2 abc

-- NB: Unlike the other properties, @distributive@ is not possible to
--  implement using only @proj1, proj2, conjunct@ and
--  it is /necessary/ to use the internal @Coercion@ of the @Sub@.