{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Newtype.LatticeProperties where

import Prelude hiding (id, (.))
import Control.Category ( Category(id, (.)) )
import Data.Type.Coercion ( Coercion() )
import Data.Type.Coercion.Sub (Sub, equiv)

import Newtype.Union ( IsUnion )        
import Newtype.Intersection ( IsIntersection ) 
import qualified Newtype.Union        as U
import qualified Newtype.Intersection as I

-- * Union types form semilattice.

idempU :: IsUnion x x z -> Coercion x z
idempU = U.unique U.idemp

commU :: IsUnion x y z -> IsUnion y x z' -> Coercion z z'
commU xyz = U.unique (U.commutative xyz)

assocU :: IsUnion x y xy -> IsUnion xy z xyz1
  -> IsUnion y z yz -> IsUnion x yz xyz2
  -> Coercion xyz1 xyz2
assocU xy xyz1 yz = U.unique (U.associative xy xyz1 yz)

monotoneU :: IsUnion x y z
  -> IsUnion x' y' z'
  -> Sub x x' -> Sub y y' -> Sub z z'
monotoneU isxyz isx'y'z' x_x' y_y' =
  U.elim isxyz (U.inl isx'y'z' . x_x') (U.inr isx'y'z' . y_y')

-- * Intersection types form semilattice.

idempI :: IsIntersection x x z -> Coercion x z
idempI = I.unique I.idemp

commI :: IsIntersection x y z -> IsIntersection y x z' -> Coercion z z'
commI xyz = I.unique (I.commutative xyz)

assocI :: IsIntersection x y xy -> IsIntersection xy z xyz1
  -> IsIntersection y z yz -> IsIntersection x yz xyz2
  -> Coercion xyz1 xyz2
assocI xy xyz1 yz = I.unique (I.associative xy xyz1 yz)

monotoneI :: IsIntersection x y z
  -> IsIntersection x' y' z'
  -> Sub x x' -> Sub y y' -> Sub z z'
monotoneI isxyz isx'y'z' x_x' y_y' =
  I.conjunct isx'y'z' (x_x' . I.proj1 isxyz) (y_y' . I.proj2 isxyz)

-- * Union and intersection types form distributive lattice.

-- | Absorption law.
--
-- > (x ∧ y ≈ xy) -> (xy ∨ x ≈ xyx) -> x ≈ xyx
-- > x ≈ (x ∧ y) ∨ x
absorbU :: IsIntersection x y xy -> IsUnion xy x xyx -> Coercion x xyx
absorbU xy = U.unique (U.greater (I.proj1 xy))

-- | Absorption law.
--
-- > (x ∨ y ≈ xy) -> (x ∧ xy ≈ xxy) -> x ≈ xxy
-- > x ≈ x ∧ (x ∨ y)
absorbI :: IsUnion x y xy -> IsIntersection x xy xxy -> Coercion x xxy
absorbI xy = I.unique (I.lesser (U.inl xy))

-- | Distributivity (Intersection over Union).
--
-- > (x ∨ y) ∧ z ≈ (x ∧ z) ∨ (y ∧ z)
distIU :: forall x y z xy xyz xz yz xzyz.
     IsUnion x y xy -> IsIntersection xy z xyz
  -> IsIntersection x z xz -> IsIntersection y z yz -> IsUnion xz yz xzyz
  -> Coercion xyz xzyz
distIU isxy isxyz isxz isyz isxzyz = equiv xyz_xzyz xzyz_xyz
  where
    xz_xyz :: Sub xz xyz
    xz_xyz = monotoneI isxz isxyz (U.inl isxy) id

    yz_xyz :: Sub yz xyz
    yz_xyz = monotoneI isyz isxyz (U.inr isxy) id

    xzyz_xyz :: Sub xzyz xyz
    xzyz_xyz = U.elim isxzyz xz_xyz yz_xyz

    xyz_xzyz :: Sub xyz xzyz
    xyz_xzyz = U.distributive isxy (I.proj1 isxyz) body

    xyz_z :: Sub xyz z
    xyz_z = I.proj2 isxyz

    body :: forall p q. Sub p x -> Sub q y -> IsUnion p q xyz -> Sub xyz xzyz
    body p_x q_y ispq_xyz = U.elim ispq_xyz p_xzyz q_xzyz
      where
        p_z :: Sub p z
        p_z = xyz_z . U.inl ispq_xyz
        
        q_z :: Sub q z
        q_z = xyz_z . U.inr ispq_xyz

        p_xzyz :: Sub p xzyz
        p_xzyz = U.inl isxzyz . I.conjunct isxz p_x p_z

        q_xzyz :: Sub q xzyz
        q_xzyz = U.inr isxzyz . I.conjunct isyz q_y q_z

-- | Distributivity (Union over Intersection).
--
-- > (x ∧ y) ∨ z ≈ (x ∨ z) ∧ (y ∨ z)
distUI :: forall x y z xy xyz xz yz xzyz.
     IsIntersection x y xy -> IsUnion xy z xyz
  -> IsUnion x z xz -> IsUnion y z yz -> IsIntersection xz yz xzyz
  -> Coercion xyz xzyz
distUI isxy isxyz isxz isyz isxzyz = equiv xyz_xzyz xzyz_xyz
  where
    xyz_xz :: Sub xyz xz
    xyz_xz = monotoneU isxyz isxz (I.proj1 isxy) id

    xyz_yz :: Sub xyz yz
    xyz_yz = monotoneU isxyz isyz (I.proj2 isxy) id

    xyz_xzyz :: Sub xyz xzyz
    xyz_xzyz = I.conjunct isxzyz xyz_xz xyz_yz

    xzyz_xyz :: Sub xzyz xyz
    xzyz_xyz = I.distributive isxy (U.inl isxyz) body

    z_xyz :: Sub z xyz
    z_xyz = U.inr isxyz

    body :: forall p q. Sub x p -> Sub y q -> IsIntersection p q xyz -> Sub xzyz xyz
    body x_p y_q ispq_xyz = I.conjunct ispq_xyz xzyz_p xzyz_q
      where
        z_p :: Sub z p
        z_p = I.proj1 ispq_xyz . z_xyz
        
        z_q :: Sub z q
        z_q = I.proj2 ispq_xyz . z_xyz

        xzyz_p :: Sub xzyz p
        xzyz_p = U.elim isxz x_p z_p . I.proj1 isxzyz

        xzyz_q :: Sub xzyz q
        xzyz_q = U.elim isyz y_q z_q . I.proj2 isxzyz
