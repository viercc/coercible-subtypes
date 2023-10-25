{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{- |

This module exposes internals of "Data.Type.Coercion.Related".

Using this module allows to violate the premises 'Related' type provides.
It is advisable not to import this module if there is another way,
and to limit the amount of code accesible to this module.

-}
module Data.Type.Coercion.Related.Internal where

import           Control.Category
import           Prelude                    hiding (id, (.))

import           Data.Coerce
import           Data.Type.Coercion

-- | @Related a b@ witnesses @a@ and @b@ shares the same runtime representation,
--   but nothing about whether @a@ can be safely coerced to or from @b@.
-- 
-- You can make 'Related' witnesses by using combinators in this module, or the methods of
-- the @'Category' Related@ instance: 'id' and @('.')@.
newtype Related (a :: k) (b :: k) = Related { getRelated :: Coercion a b }
  deriving stock (Eq, Ord, Show)
-- It is intentional to omit the 'TestCoercion' instance, existing for @Coercion@.
-- Knowing @Related a b@ and @Related a c@ should not conclude
-- @Coercible b c@.

deriving stock instance Coercible a b => Read (Related a b)
deriving newtype instance Coercible a b => Enum (Related a b)
deriving newtype instance Coercible a b => Bounded (Related a b)

instance Category Related where
  id :: Related a a
  id = Related Coercion

  (.) :: Related b c -> Related a b -> Related a c
  Related Coercion . Related Coercion = Related Coercion
