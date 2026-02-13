module NewType where

import Control.Exception (assert)
import Data.Typeable (typeOf, typeRep)

-- Only exactly 1 constructor with exactly 1 field
-- newtype Test2 = Test2 -- A newtype constructor must have exactly one field
--     deriving (Show)
-- Yet newtype is lighter than data
newtype MyIntN = MyIntN {fromMyN :: Int}
    deriving (Show)

-- data can have multiple fields inside multiple constructors
data MyIntD
    = MyIntD {fromMyD :: Int, inner :: Int}
    | MyIntD2 Int
    | MyIntD3
    deriving (Show)

testNewT :: IO ()
testNewT = do
    let n1 = MyIntN 1
    let n2 = MyIntN 2
    print n1
    -- print $ n1 + n2 -- can't do because MyIntN does not instanciate Num

    let d1 = MyIntD 1 2
    let d2 = MyIntD 2 3
    print d1

    let d21 = MyIntD2 5
    let d31 = MyIntD3
    print d21
    print d31

-- print $ d1 + d2 -- same

-- https://wiki.haskell.org/Newtype#The_messy_bits

-- https://wiki.haskell.org/index.php?title=Bottom
bottom = bottom

-- i'll use undefined because it does not cause infinite loop

newtype NTBool = NTBool {fromNT :: Bool}
    deriving (Eq, Show)
data DBool = DBool {fromD :: Bool}
    deriving (Eq, Show)

testIsomorphy :: IO ()
testIsomorphy = do
    -- newtype
    assert ((NTBool . fromNT $ NTBool True) == NTBool True) (print "1 Ok")
    assert ((NTBool . fromNT $ NTBool False) == NTBool False) (print "2 Ok")
    print $ typeOf (NTBool . fromNT $ NTBool undefined) -- NTBool is expected
    print $ typeOf (NTBool . fromNT $ undefined) -- NTBool is NOT expected but received

    -- data
    assert ((DBool . fromD $ DBool True) == DBool True) (print "1 Ok")
    assert ((DBool . fromD $ DBool False) == DBool False) (print "2 Ok")
    print $ typeOf (DBool . fromD $ DBool undefined) -- DBool is expected
    print $ typeOf (DBool . fromD $ undefined) -- DBool is NOT expected but received

    -- Wow, no crash
    case undefined of
        NTBool _ -> print "I am NTBool"
        _ -> print "I am not"

    -- Data
    -- Crashes because of evaluating undefined
    case undefined of
        DBool _ -> print "I am DBool"
        _ -> print "I am not"
