{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PolyKinds             #-}

{- |

This module exposes internals of "Data.Type.Coercion.Sub".

Using this module allows to violate the premises 'Sub' type provides.
It is advisable not to import this module if there is another way,
and to limit the amount of code accesible to this module.

-}
module Data.Type.Coercion.Sub.Internal(
  Sub(..)
) where

import           Control.Category
import           Prelude                    hiding (id, (.))

import           Data.Type.Coercion

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

instance Category Sub where
  id :: Sub a a
  id = Sub Coercion

  (.) :: Sub b c -> Sub a b -> Sub a c
  Sub Coercion . Sub Coercion = Sub Coercion
