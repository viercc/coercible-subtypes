{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Newtype.Intersection(
  module Data.Type.Coercion.Related,
  IsIntersection(..),
  withIntersection,
  
  unique, lesser, idemp, commutative, associative
) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion.Sub (equiv, sub)
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion.Related
import Data.Type.Coercion.Related.Internal
import Data.Type.Coercion ( Coercion(Coercion), TestCoercion(..) )

-- | @IsIntersection x y z@ witnesses the fact:
--
--   * All @x, y, z@ share the same runtime representation
--   * @z@ is an intersection type of @x@ and @y@. In other words, the following three holds:
--
--       * @'Sub' z x@
--       * @Sub z y@
--       * For any type @s@ satisfying both of @(Sub s x, Sub s y)@, @Sub s z@.
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

-- | For a pair of 'Related' types @x@ and @y@, make some (existentially quantified)
--   type @xy@ where @xy@ is an intersection type of @x, y@.
withIntersection :: Related x y -> (forall xy. IsIntersection x y xy -> r) -> r
withIntersection (Related Coercion) body =
    body IsIntersection{ proj1 = sub, proj2 = id, conjunct = seq }

-- | Two intersection types @z,z'@ of the same pair of types @x,y@ may be different,
--   but they are equivalent in terms of coercibility.
unique :: IsIntersection x y z -> IsIntersection x y z' -> Coercion z z'
unique xy xy' = equiv (conjunct xy' (proj1 xy) (proj2 xy)) (conjunct xy (proj1 xy') (proj2 xy'))

-- | When @Sub x y@, @x@ itself is an intersection type of @x, y@.
lesser :: Sub x y -> IsIntersection x y x
lesser l = IsIntersection{ proj1=id, proj2=l, conjunct= \sx !_ -> sx }


-- | Intersection is idempotent.
--
--   Note: combining @idemp@ and 'unique', @IsIntersection x x z -> Coercion x z@ holds.
idemp :: IsIntersection x x x
idemp = lesser id

-- | Intersection is commutative.
--
--   Note: combining @commutative@ and 'unique', @IsIntersection x y xy -> IsIntersection y x yx -> Coercion xy yx@ holds.
commutative :: IsIntersection x y z -> IsIntersection y x z
commutative xyz = IsIntersection{ proj1 = proj2 xyz, proj2 = proj1 xyz, conjunct = flip (conjunct xyz)}

-- | Intersection is associative.
--
--   Note: combining @associative@ and 'unique', the following holds.
--   
--   >    IsIntersection x y xy -> IsIntersection xy z xy'z
--   > -> IsIntersection y z yz -> IsIntersection x yz x'yz
--   > -> Coercion xy'z x'yz 
associative :: IsIntersection x y xy -> IsIntersection xy z xyz -> IsIntersection y z yz -> IsIntersection x yz xyz
associative xy xy'z yz =
    IsIntersection {
       proj1 = proj1 xy . proj1 xy'z
     , proj2 = conjunct yz (proj2 xy . proj1 xy'z) (proj2 xy'z)
     , conjunct = \s_x s_yz -> conjunct xy'z (conjunct xy s_x (proj1 yz . s_yz)) (proj2 yz . s_yz)
    }
