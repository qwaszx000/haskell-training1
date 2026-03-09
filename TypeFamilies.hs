{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TypeFamilies where

import Data.Foldable (foldl')
import Data.Kind (Type)

data Nat = Z | S Nat

type family NPlus (x :: Nat) (y :: Nat) :: Nat where
    NPlus Z y = y
    -- NPlus x Z = x
    NPlus (S x) y = S (NPlus x y)

data Vec :: Nat -> Type -> Type where
    Nil :: Vec Z a
    Cons :: a -> Vec n a -> Vec (S n) a

instance (Show a) => Show (Vec n a) where
    show :: (Show a) => Vec n a -> String
    show vec = "<" <> subshow vec <> ">"
      where
        subshow :: Vec n1 a -> String
        subshow Nil = ""
        subshow (Cons x Nil) = show x
        subshow (Cons x xs) = show x <> ", " <> subshow xs

instance Foldable (Vec n) where
    foldr :: (a -> b -> b) -> b -> Vec n a -> b
    foldr _ acc Nil = acc
    foldr f acc (Cons x xs) = foldr f (f x acc) xs

instance Functor (Vec n) where
    fmap :: (a -> b) -> Vec n a -> Vec n b
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

vjoin :: Vec n1 a -> Vec n2 a -> Vec (NPlus n1 n2) a
vjoin Nil ys = ys
vjoin (Cons x xs) ys = Cons x (vjoin xs ys)

class Box b where
    type Elem b :: Type
    data DElem b
    belem :: b -> Elem b
    delem :: b -> DElem b

instance Box Char where
    type Elem Char = Int
    data DElem Char = DNone | DSome Char

    belem :: Char -> Int
    belem _ = 5

    delem :: Char -> DElem Char
    delem = DSome

instance Box Int where
    type Elem Int = String
    data DElem Int = DINone | DISome Char

    belem i = "Unexpected? = " <> show i
    delem _ = DISome 'c'

-- Creates new type
-- Data families are always open(can't use "where")
data family DFTest1 a

data instance DFTest1 Int = DFInt Int (DFTest1 Int) | DFIntNil
newtype instance DFTest1 Char = DFChar Int

dftestFun :: DFTest1 Int -> DFTest1 Char
dftestFun DFIntNil = DFChar 5
dftestFun (DFInt x xs) = DFChar x

-- Different type
-- dftestFun (DFChar c) = _

-- Must be inside class instance declaration
-- data instance DElem Char = DElemMy Int

-- Creates type synonym
-- Same as NPlus is synonymous to Nat
-- Closed family because of where
type family TFTest1 something where
    --  But Closed families allow us to do sequential pattern matching
    --  As in TypeName (Something c) = x
    --  TypeName (Nothing x) = y
    --  TypeName _ = u
    --  Can't do the same with open families(because they can't guarantee order and/or totality of matching)
    --  TFTest1 a = a
    --  TFTest1 _ = Int
    TFTest1 Int = String

-- We can't add instance because family is closed
-- type instance TFTest1 Float = Char

-- Open family(no "where")
type family TFTest2 smth
type instance TFTest2 Float = Char

-- Can be extended
type instance TFTest2 Int = Char

-- Basically TFTest2 Int == Char
-- ghci> let a = 'c' :: TFTest2 Float
-- ghci> :t a
-- a :: Char
-- ghci> :t a :: TFTest2 Float
-- a :: TFTest2 Float :: Char
-- ghci> :t a :: TFTest2 Int
-- a :: TFTest2 Int :: Char

testBox :: IO ()
testBox = do
    let x = map belem "hello"
    print x
    let x' = map belem x
    print x'
    let superx = concatMap (map belem) x'
    print superx
    pure ()

-- https://serokell.io/blog/type-families-haskell

type Append :: [a] -> [a] -> [a]
type family Append xs ys where
    -- type family Append (xs :: [a]) (ys :: [a]) :: [a] where
    Append '[] ys = ys
    Append (x : xs) ys = x : Append xs ys

-- ghci> :k '[]
-- '[] :: [a]
--
-- ghci> :kind Append [1,2,3] [4,5,6]
-- Append [1,2,3] [4,5,6] :: [GHC.Num.Natural.Natural]
-- ghci> :kind! Append [1,2,3] [4,5,6]
-- Append [1,2,3] [4,5,6] :: [GHC.Num.Natural.Natural]
-- = [1, 2, 3, 4, 5, 6]

-- Can't i use it?
-- myappend :: Append a b c
-- myappend xs ys = xs
