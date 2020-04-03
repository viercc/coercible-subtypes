{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
module Data.Type.Coercion.Sub(
  Sub(), sub, toSub, upcastWith, equiv, gequiv,

  coercionIsSub,

  mapR, contramapR,
  bimapR, dimapR
) where

import           Control.Category
import           Prelude                    hiding (id, (.))

import           Data.Coerce
import           Data.Type.Coercion

import           Data.Bifunctor             (Bifunctor)
import           Data.Functor.Contravariant (Contravariant)
import           Data.Profunctor            (Profunctor)

{- | @Sub a b@ witnesses a zero-cost conversion @a -> b@.

@Sub@ is a newtype wrapper around 'Coercion', but made opaque to hide
the ability to 'coerce' into other-direction.

This is convenient for newtype wrappers which give additional guarantee.

As an example, think about the following code:

> -- | A pair @(x::a, y::a)@, but guaranteed @x <= y@
> newtype Range a = MkRange (a,a)
>
> getRange :: Range a -> (a,a)
> getRange = coerce
> mkRange :: Ord a => a -> a -> Range a
> mkRange x y = if x <= y then MkRange (x,y) else MkRange (y,x)

If you want to provide this type from a library you maintain,
you would want to keep @Range@ abstract from outside of the module.

An user may want to convert @[Range a]@ to @[(a,a)]@ without actually
traversing the list. This is possible if the user have access to the
internals, or you export a @Coercion (Range a) (a,a)@ value. But doing so
breaks the guarantee, because it also allows to use @Coercible@ in the other
direction, as in @coerce (10,5) :: Range Int@.

By exporting only @Sub (Range a) (a,a)@ value from your module,
this user can get @Sub [Range a] [(a,a)]@ using 'mapR'.

-}
newtype Sub (a :: k) (b :: k) = Sub { getSub :: Coercion a b }
  deriving (Eq, Ord, Show)
-- It is intentional to omit some instances.
--
-- TestCoercion instance should not exist.
-- Knowing `Sub a b` and `Sub a c` should not conclude
-- `Coercible b c`.
--
-- Among instances `Coercion` has, Enum, Bounded, and Read are
-- excluded because they allows to make new value of `Sub a b`.
-- Constructing `Sub a b` values must be done through
-- combinators provided by this module or exported for
-- abstract type under library author's careful choice.

-- | Make a witness for type-safe casting which respects direction.
sub :: Coercible a b => Sub a b
sub = Sub Coercion

toSub :: Coercion a b -> Sub a b
toSub = Sub

-- | Type-safe cast
upcastWith :: Sub a b -> a -> b
upcastWith (Sub Coercion) = coerce

-- | All 'Coercion' can be seen as 'Sub'
coercionIsSub :: Sub (Coercion a b) (Sub a b)
coercionIsSub = Sub Coercion

instance Category Sub where
  id :: Sub a a
  id = Sub Coercion

  (.) :: Sub b c -> Sub a b -> Sub a c
  Sub Coercion . Sub Coercion = Sub Coercion

-- | `Sub` relation in both direction means there is `Coercion` relation.
equiv :: Sub a b -> Sub b a -> Coercion a b
equiv ab ba = gequiv ab ba Coercion

-- | Generalized 'equiv'
gequiv :: Sub a b -> Sub b a -> (Coercible a b => r) -> r
gequiv (Sub Coercion) (Sub Coercion) k = k

{-

Note: evaluating both arguments of `equiv` is necessary.
One might notice the following typechecks.

    equiv :: Sub a b -> Sub b a -> Coercion a b
    equiv (Sub Coercion) _ = Coercion

But this implementation allows inverting `Sub a b` circumventing the restriction;

    bad :: Sub a b -> Sub b a
    bad ab =
      let ba = upcastWith coercionIsSub (equiv ab ba)
      in ba

This is prevented by evaluating both arguments of `equiv`, making `bad ab` a bottom.

-}

-----------------------------

-- | Extend subtype relation covariantly.
mapR :: ( forall x x'. Coercible x x' => Coercible (t x) (t x')
        , Functor t)
     => Sub a b -> Sub (t a) (t b)
mapR (Sub Coercion) = Sub Coercion

-- | Extend subtype relation contravariantly
contramapR :: ( forall x x'. Coercible x x' => Coercible (t x) (t x')
              , Contravariant t)
           => Sub a b -> Sub (t b) (t a)
contramapR (Sub Coercion) = Sub Coercion

bimapR :: ( forall x x' y y'.
              (Coercible x x', Coercible y y') => Coercible (t x y) (t x' y')
          , Bifunctor t)
       => Sub a a' -> Sub b b' -> Sub (t a b) (t a' b')
bimapR (Sub Coercion) (Sub Coercion) = Sub Coercion

dimapR :: ( forall x x' y y'.
              (Coercible x x', Coercible y y') => Coercible (t x y) (t x' y')
          , Profunctor t)
       => Sub a a' -> Sub b b' -> Sub (t a' b) (t a b')
dimapR (Sub Coercion) (Sub Coercion) = Sub Coercion
