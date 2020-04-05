# coercible-subtypes

This library provides unidirectional variant of (one-way) [Coercion](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Coercion.html).

The variant is a type `Sub` defined in `Data.Type.Coercion.Sub`.
`Sub a b` can be used to convert a type `a` to another type `b`.

```
upcastWith :: Sub a b -> a -> b
```

For all `Sub a b` values, the runtime representation of `a` and
`b` values are same, so `upcastWith` do not require any computation
to return `b` value, just [coerce](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Coerce.html)s
GHC to treat a value of `a` as type `b`.
This feature is not different to `Coercion`.

The difference is that while `Coercion` represents
bidirectional relation, `Sub` represents unidirectional relation.
`Coercion a b` and its underlying type class `Coercible a b` witnesses you can coerce both `a` to `b` and `b` to `a`.
Unlike that, `Sub a b` only allow you to coerce `a` to `b`, not `b` to `a`.
## Example

To use this library effectively, it must be used at two places: a library
and its user code. For this example, let's assume they are written by two people,
a *library author* and a *user*.

The library author writes a module `RightTriangle` below.

```
module RightTriangle(Triangle(), fromEdges, getEdges) where
  newtype Triangle = MkTriangle (Int, Int, Int)
  
  -- | Creates right triangle from lengths of edges (a,b,c)
  --   
  --   >  *
  --   >  |\ c
  --   > a| \
  --   >  *--*
  --   >   b
  fromEdges :: (Int, Int, Int) -> Maybe Triangle
  
  getEdges :: Triangle -> (Int, Int, Int)
  getEdges (MkTriangle edges) = edges
```
