{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{- |
  'Related' type to witness that two types @a@ and @b@ shares the same runtime representation,
  but nothing about whether @a@ can be safely coerced to or from @b@.

  Unlike 'Coercion' or 'Sub', having a value of @Related a b@ does not allow to
  touch values of @a@ or @b@.

  This module is used alonside "Newtype.Union" and "Newtype.Intersection"
  to define union and intersection types of two 'Related' types.
-}
module Data.Type.Coercion.Related(
    Related(),
    related,
    subIsRelated, coercionIsRelated,
    symRelated, undirected, informRelation
) where

import Data.Coerce
import Data.Type.Coercion
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion.Related.Internal

-- | @'Coercible' a b@ implies @Related a b@.
related :: Coercible a b => Related a b
related = Related Coercion

-- | @'Sub' a b@ implies @Related a b@.
subIsRelated :: Sub (Sub a b) (Related a b)
subIsRelated = sub

-- | @'Coercion' a b@ implies @'Sub' a b@, which implies @Related a b@.
coercionIsRelated :: Sub (Coercion a b) (Related a b)
coercionIsRelated = sub

-- | @Related@ is a symmetric relation.
symRelated :: Related a b -> Related b a
symRelated ab = informRelation ab related

-- | A direct consequence of 'informRelation' and 'subIsRelated'.
undirected :: Sub a b -> (Coercible a b => Related x y) -> Related x y
undirected ab = informRelation (upcastWith subIsRelated ab)

-- | Given @Related a b@, you can assume @Coercible a b@ for the purpose of proving
--   another @Related@ relation.
informRelation :: Related a b -> (Coercible a b => Related x y) -> Related x y
informRelation (Related Coercion) body = body