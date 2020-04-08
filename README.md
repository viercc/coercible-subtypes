# coercible-subtypes

This library provides unidirectional (one-way) variant of [Coercion](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Coercion.html).

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
`Coercion a b` and its underlying type class `Coercible a b` witnesse you can coerce both `a` to `b` and `b` to `a`.
Unlike that, `Sub a b` only allows you to coerce `a` to `b`, not `b` to `a`.

## Usage Example

To use this library effectively, it must be used at two places: a library
and its user code. For this example, let's assume they are written by two people,
a *library author* and a *user*.

The library author writes a module `RightTriangle` below.

```
module RightTriangle(Triangle(), toEdges, getEdges, fromEdges) where
  import Data.Coerce
  import Data.Type.Coercion.Sub
  
  newtype Triangle = MkTriangle (Int, Int, Int)
  
  -- | Triangles can be coerced into 3-tuples of Ints
  toEdges :: Sub Triangle (Int, Int, Int)
  toEdges = sub
  
  getEdges :: Triangle -> (Int, Int, Int)
  getEdges = coerce
  
  -- | Creates right triangle from lengths of edges (a,b,c)
  -- 
  -- >  *
  -- >  |\ c
  -- > a| \
  -- >  *--*
  -- >   b
  --
  -- (a^2 + b^2 == c^2) must hold.
  fromEdges :: (Int, Int, Int) -> Maybe Triangle
  fromEdges = {- Omit -}
```

The author wants to protect the invariant condition `a^2 + b^2 == c^2`.
For that purpose, the author can't export the constructor of `Triangle`.
Because it is symmetric, `Coercion Triangle (Int,Int,Int)` can't be exported either.

The user is building an application using `RightTriangle` module.

```
module Main where
  import Data.Map (Map)
  import RightTriangle
  
  import Data.Type.Coercion.Sub
  
  main :: IO ()
  main = ......
```

In this application, the user has to convert `Map String Triangle` to
`Map String (Int, Int, Int)`, revealing the edge lengths of the triangles.
While it is easy to do so with `fmap getEdges`,
using `fmap` here can make an entire copy of the Map<sup>[†](#footnote)</sup>.
This is wasted work and memory. Instead, the user can use `mapR toEdges` to get
`Sub (Map String Triangle) (Map String (Int, Int, Int))`
and then `upcastWith` to perform zero cost coercion over `Map`.

## Comparison against other methods

There are some other methods to achive the goal of this library.

* Just give up coercion

  * This is just for better performance, so not doing it
    is always an option.

* Rewrite rules

  * Rewrite rules based method is currently employed, and working at our hand.
    So, it is possible you don't need this library at all.
  
  * The downside is whether it works or not is on the provider of the
    "container" type in use, and GHC doing expected optimizations.
    Without reading source codes and examining the GHC optimization result (e.g. `-ddump-rule-firings`),
    you can't be sure you are doing the conversion zero-cost.

--------

<a id="footnote">†</a> For `Data.Map`, which [containers](https://hackage.haskell.org/package/containers)
package provides, can optimize `fmap` away via proper inlining and rewrite rules. The purpose of this library
is turning optimizations into explicit codes, or handling the cases when the container type in use does not
provide such an opportunity via rewrite rules.</small>
