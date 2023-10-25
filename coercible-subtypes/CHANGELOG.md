# Revision history for coercible-subtypes

## 0.3.0.0 -- 2023-01-01

* Breaking changes
  
  * Change the `Newtype.Intersection.associative` and `Newtype.Union.associative` to new,
    more succinct type.

* Instance added to `IsIntersection` and `IsUnion`

## 0.2.0.0 -- 2021-09-13

* Add `Related`
* Add `IsIntersection` and `IsUnion` facilities

## 0.1.1.0 -- 2021-08-24

* Add instances `Coercible a b => (Read (Sub a b), Enum (Sub a b), Bounded (Sub a b))`
* Add `instantiate`
* Add some more combinators

## 0.1.0.0 -- 2020-04-08

* First version.
