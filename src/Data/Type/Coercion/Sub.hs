{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{- | @'Sub' a b@ witnesses a zero-cost conversion @a -> b@.

=== Example

Think about the following code:

> -- | A pair @(x::a, y::a)@, but guaranteed @x <= y@
> newtype Range a = MkRange (a,a)
>
> getRange :: Range a -> (a,a)
> getRange = coerce
> mkRange :: Ord a => a -> a -> Range a
> mkRange x y = if x <= y then MkRange (x,y) else MkRange (y,x)

If you want to provide this type from a library you maintain,
you would want to keep @Range@ abstract from outside of the module.

A user may want to convert @[Range a]@ to @[(a,a)]@ without actually
traversing the list. This is possible if the user have access to the
internals, or you export a @Coercion (Range a) (a,a)@ value. But doing so
breaks the guarantee, because it also allows to use @Coercible@ in the other
direction, as in @coerce (10,5) :: Range Int@.

By exporting only @Sub (Range a) (a,a)@ value from your module,
this user can get @Sub [Range a] [(a,a)]@ using 'mapR',
without being able to make an invalid value.

-}
module Data.Type.Coercion.Sub(
  Sub(),
  sub, toSub, upcastWith, equiv, gequiv,

  coercionIsSub,

  instantiate,
  mapR, contramapR,
  bimapR, prodR, prod3R, sumR, dimapR, arrR
) where

import           Data.Coerce
import           Data.Type.Coercion

import           Data.Bifunctor                  (Bifunctor)
import           Data.Functor.Contravariant      (Contravariant)
import           Data.Profunctor                 (Profunctor)

import           Data.Type.Coercion.Sub.Internal

-- | Make a directed witness of @'coerce' :: a -> b@.
sub :: Coercible a b => Sub a b
sub = Sub Coercion

-- | Make a directed witness of @'coerce' :: a -> b@, from a 'Coercion' value.
toSub :: Coercion a b -> Sub a b
toSub = Sub

-- | Type-safe cast
upcastWith :: Sub a b -> a -> b
upcastWith (Sub Coercion) = coerce

-- | All 'Coercion' can be seen as 'Sub'
coercionIsSub :: Sub (Coercion a b) (Sub a b)
coercionIsSub = Sub Coercion

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

-- | For a @Sub@ relation between type constructors @f@ and @g@,
--   create an instance of subtype relation @Sub (f a) (g a)@ for any type
--   parameter @a@.
instantiate :: forall j k (f :: j -> k) (g :: j -> k) (a :: j).
  Sub f g -> Sub (f a) (g a)
instantiate (Sub Coercion) = sub

-- | Extend subtype relation covariantly.
mapR :: ( forall x x'. Coercible x x' => Coercible (t x) (t x')
        , Functor t)
     => Sub a b -> Sub (t a) (t b)
mapR (Sub Coercion) = Sub Coercion

-- | Extend subtype relation contravariantly.
contramapR :: ( forall x x'. Coercible x x' => Coercible (t x) (t x')
              , Contravariant t)
           => Sub a b -> Sub (t b) (t a)
contramapR (Sub Coercion) = Sub Coercion

-- | Extend subtype relation over a 'Bifunctor'.
bimapR :: ( forall x x' y y'.
              (Coercible x x', Coercible y y') => Coercible (t x y) (t x' y')
          , Bifunctor t)
       => Sub a a' -> Sub b b' -> Sub (t a b) (t a' b')
bimapR (Sub Coercion) (Sub Coercion) = Sub Coercion

-- | 'bimapR' specialized for the pair type '(a,b)'
prodR :: Sub a a' -> Sub b b' -> Sub (a,b) (a',b')
prodR = bimapR

infixr 3 `prodR`

-- | 'bimapR' specialized for 'Either'
sumR :: Sub a a' -> Sub b b' -> Sub (Either a b) (Either a' b')
sumR = bimapR

infixr 2 `sumR`

-- | Extend subtype relation over the 3-tuple types '(a,b,c)'
prod3R :: Sub a a' -> Sub b b' -> Sub c c' -> Sub (a,b,c) (a',b',c')
prod3R (Sub Coercion) (Sub Coercion) (Sub Coercion) = Sub Coercion

-- | Extend subtype relation over a 'Profunctor'.
dimapR :: ( forall x x' y y'.
              (Coercible x x', Coercible y y') => Coercible (t x y) (t x' y')
          , Profunctor t)
       => Sub a a' -> Sub b b' -> Sub (t a' b) (t a b')
dimapR (Sub Coercion) (Sub Coercion) = Sub Coercion

-- | 'dimapR' specialized for functions @(->)@
arrR :: Sub a a' -> Sub b b' -> Sub (a' -> b) (a -> b')
arrR = dimapR

infixr 1 `arrR`
