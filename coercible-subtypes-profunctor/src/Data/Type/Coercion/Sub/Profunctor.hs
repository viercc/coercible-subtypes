{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Data.Type.Coercion.Sub.Profunctor
  ( dimapR,
    lmapR,
    rmapR,
  )
where

import Data.Coerce (Coercible)
import Data.Profunctor (Profunctor ())
import Data.Type.Coercion (Coercion (..))
import Data.Type.Coercion.Sub.Internal

-- | Extend subtype relation over a 'Profunctor'.
--
-- @
-- 'Data.Type.Coercion.Sub.upcastWith' ('dimapR' f g) == 'Data.Profunctor.dimap' (upcastWith f) (upcastWith g)
-- @
dimapR ::
  ( forall x x' y y'.
    (Coercible x x', Coercible y y') =>
    Coercible (t x y) (t x' y'),
    Profunctor t
  ) =>
  Sub a a' ->
  Sub b b' ->
  Sub (t a' b) (t a b')
dimapR (Sub Coercion) (Sub Coercion) = Sub Coercion

-- | Extend subtype relation over a 'Profunctor' (, but only contravariant part.)
--
-- @
-- 'Data.Type.Coercion.Sub.upcastWith' ('lmapR' f) == 'Data.Profunctor.lmap' (upcastWith f)
-- @
lmapR ::
  ( forall x x' y.
    (Coercible x x') =>
    Coercible (t x y) (t x' y),
    Profunctor t
  ) =>
  Sub a a' ->
  Sub (t a' b) (t a b)
lmapR (Sub Coercion) = Sub Coercion

-- | Extend subtype relation over a 'Profunctor' (, but only covariant part.)
--
-- @
-- 'Data.Type.Coercion.Sub.upcastWith' ('rmapR' g) == 'Data.Profunctor.rmap' (upcastWith g)
-- @
rmapR ::
  ( forall x y y'.
    (Coercible y y') =>
    Coercible (t x y) (t x y'),
    Profunctor t
  ) =>
  Sub b b' ->
  Sub (t a b) (t a b')
rmapR (Sub Coercion) = Sub Coercion
