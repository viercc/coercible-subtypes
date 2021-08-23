{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import           Data.Coerce
import           Data.Type.Coercion

newtype Sub (a :: k) (b :: k) = Sub { getSub :: Coercion a b }
  deriving stock (Eq, Ord, Show)
-- It is intentional to omit the 'TestCoercion' instance, existing for @Coercion@.
-- Knowing @Sub a b@ and @Sub a c@ should not conclude
-- @Coercible b c@.

deriving stock instance Coercible a b => Read (Sub a b)
deriving newtype instance Coercible a b => Enum (Sub a b)
deriving newtype instance Coercible a b => Bounded (Sub a b)

instance Category Sub where
  id :: Sub a a
  id = Sub Coercion

  (.) :: Sub b c -> Sub a b -> Sub a c
  Sub Coercion . Sub Coercion = Sub Coercion
