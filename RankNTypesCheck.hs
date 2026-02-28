{-# LANGUAGE RankNTypes #-}

module RankNTypesCheck where

testFun :: (forall x. x -> Int) -> (forall a. a -> Int)
testFun f = f
