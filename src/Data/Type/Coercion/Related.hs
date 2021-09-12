{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{- | @Related a b@ witnesses that @a@ and @b@ shares the same runtime representation,
     but nothing about whether @a@ can be safely coerced to or from @b@.
-}
module Data.Type.Coercion.Related(
    Related(), related,
    subIsRelated, coercionIsRelated,
    symRelated, undirected, informRelation
) where

import Data.Coerce
import Data.Type.Coercion
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion.Related.Internal

related :: Coercible a b => Related a b
related = Related Coercion

subIsRelated :: Sub (Sub a b) (Related a b)
subIsRelated = sub

coercionIsRelated :: Sub (Coercion a b) (Related a b)
coercionIsRelated = sub

symRelated :: Related a b -> Related b a
symRelated ab = informRelation ab related

undirected :: Sub a b -> (Coercible a b => Related x y) -> Related x y
undirected ab = informRelation (upcastWith subIsRelated ab)

informRelation :: Related a b -> (Coercible a b => Related x y) -> Related x y
informRelation (Related Coercion) body = body