# What is this?

This is an experimental Haskell library "coercible-subtypes",
which is for latest GHC (>= 8.6).

This library provides just one feature, one-way coercion. What it means?

The current implementation of [safe coercion](https://wiki.haskell.org/GHC/Coercible) in GHC
has no distinction of directions. `Coercible A B` never just means `A` is Coercible to `B`,
it's always `A` is Coercible **to and from** `B`.

This symmetry is nice in many situations, and greatly simplifies the semantics of
`GeneralizedNewtypeDeriving` and its superior brother `DerivingVia`.
But everything has an exception. Sometimes, you want weaker property on two types:
`A` is safely coercible to `B`, but not in the other way around.

The type `Sub` in `Data.Type.Coercion.Sub` represents the said one-way
coercion.

Quoting the documentation:

> `Sub` is a newtype wrapper around [Coercion](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Coercion.html#t:Coercion), but made opaque to hide
> the ability to 'coerce' into other-direction.

``` haskell
newtype Sub a b = Sub { getSub :: Coercion a b }
```

> This is convenient for newtype wrappers which give additional guarantee.
>
> As an example, think about the following code:
> 
> ``` haskell
> -- | A pair @(x::a, y::a)@, but guaranteed @x <= y@
> newtype Range a = MkRange (a,a)
> getRange :: Range a -> (a,a)
> getRange = coerce
> mkRange :: Ord a => a -> a -> Range a
> mkRange x y = if x <= y then MkRange (x,y) else MkRange (y,x)
> ```
> 
> If you want to provide this type from a library you maintain,
> you would want to keep `Range` abstract from outside of the module.
>
> An user may want to convert `[Range a]` to `[(a,a)]` without actually
> traversing the list. This is possible if the user have access to the
> internals, or you export a `Coercion (Range a) (a,a)` value. But doing so
> breaks the guarantee, because it also allows to use `Coercible` in the other
> direction, as in `coerce (10,5) :: Range Int`.
> 
> By exporting `Sub (Range a) (a,a)` value from your module,
> this user can get `Sub [Range a] [(a,a)]` using `mapR`.

The `mapR` function referenced here is:

``` haskell
-- | Extend subtype relation covariantly.
mapR :: ( forall x x'. Coercible x x' => Coercible (t x) (t x')
        , Functor t)
     => Sub a b -> Sub (t a) (t b)
mapR (Sub Coercion) = Sub Coercion
```

In this complex type signature, `forall x x'. Coercible x x' => Coercible (t x) (t x')`
just means the argument of a Functor `t` is [representational](https://downloads.haskell.org/ghc/8.8.3/docs/html/users_guide/glasgow_exts.html#roles).

You can omit the constraint `Functor t` and it will compile just fine.
But that does not mean it is unnecessary! Without it, the premise -- having `Sub a b` not necessarily mean you can convert `b -> a` -- is ruined.

``` haskell
newtype Op b a = Op { getOp :: a -> b }

mapR' :: ( forall x x'. Coercible x x' => Coercible (t x) (t x') )
     => Sub a b -> Sub (t a) (t b)

terrible :: forall a b. Sub a b -> b -> a
terrible a2b = getOp $ ouch (Op id)
  where
    ouch :: Op a a -> Op a b
    ouch = upcastWith (mapR' a2b)

deadly :: forall a b. Sub a b -> Coercion a b
deadly a2b = upcastWith (mapR' a2b) (Coercion :: Coercion a a)
```

By having `Functor t`, it only gives you what you already have been able to get (`fmap (upcastWith a2b) :: f a -> f b`),
just more efficient one.

(Technically, anyone can define an unlawful `Functor (Op b)` instance. But at that point,
doing so is more closer to `unsafeCoerce` than importing `MyModule.Internal`.)
