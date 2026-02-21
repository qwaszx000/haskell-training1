{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- Not required, because of GADTs?
-- {-# LANGUAGE KindSignatures #-}

module Datakinds where

data Nat = Z | S Nat

-- Basically DataKinds creates new kind - Nat
-- And allows it to be used here
-- KindSignatures allows specifying kind as in (len :: Nat)
-- But maybe GADTs allow it too
data Vector (len :: Nat) a where
    EVec :: Vector Z a
    VCons :: a -> Vector n a -> Vector (S n) a

instance Functor (Vector n) where
    fmap _ EVec = EVec
    fmap f (VCons val vec) = VCons (f val) (fmap f vec)
