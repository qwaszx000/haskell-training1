module MonadST where

import qualified Control.Monad.ST.Lazy as STL
import qualified Control.Monad.ST.Strict as STS
import qualified Control.Monad.ST.Unsafe as STSU

import Control.Monad (forever, mapM)
import qualified Data.STRef.Lazy as RL
import qualified Data.STRef.Strict as RS

-- Here is a cleaner realisation
-- https://wiki.haskell.org/Monad/ST#A_few_simple_examples

fibST :: Int -> STL.ST s Integer
fibST n = do
    if n < 2
        then pure $ fromIntegral n
        else do
            n1 :: RL.STRef s Integer <- RL.newSTRef 0
            n2 <- RL.newSTRef (1 :: Integer)
            fibST' (n - 2) n1 n2
  where
    fibST' :: Int -> RL.STRef s Integer -> RL.STRef s Integer -> STL.ST s Integer
    fibST' 0 n1 n2 = do
        n1v <- RL.readSTRef n1
        n2v <- RL.readSTRef n2
        pure $ n1v + n2v
    fibST' n n1 n2 = do
        n1v <- RL.readSTRef n1
        n2v <- RL.readSTRef n2
        RL.writeSTRef n2 (n1v + n2v)
        RL.writeSTRef n1 n2v
        fibST' (n - 1) n1 n2

pureFibWrap :: Int -> Integer
pureFibWrap n = STL.runST $ fibST n

testFibST :: IO ()
testFibST = do
    let x = pureFibWrap 100
    print x

------------------------
-- Inspired by
-- https://stackoverflow.com/questions/24072934/haskell-how-lazy-is-the-lazy-control-monad-st-lazy-monad

lazySTTest :: STL.ST s Int
lazySTTest = do
    x <- RL.newSTRef 5
    forever $ RL.writeSTRef x 6 -- hangs
    RL.writeSTRef x 7
    xv <- RL.readSTRef x
    -- pure 7 -- does not prevent hanging
    -- Not sure why, maybe because we are really going from then end
    -- We see pure xv and calculate xv next?
    pure xv

-- pure 7 -- Prevents hanging
-- Which is another proof of laziness

strictSTTest :: STS.ST s Int
strictSTTest = do
    -- forever $ pure () - another way
    x <- RS.newSTRef 5
    -- STSU.unsafeInterleaveST $ forever $ RS.writeSTRef x 6 -- use instead of next line to fix hanging
    forever $ RS.writeSTRef x 6
    RS.writeSTRef x 7
    xv <- RS.readSTRef x
    pure xv
    pure 5 -- Still hangs, proof of strictness

testLazyST :: IO ()
testLazyST = do
    let x = STL.runST lazySTTest
    print x

-- For some reason even can't kill it with ctrl+c in ghci
testStrictST :: IO ()
testStrictST = do
    let x = STS.runST strictSTTest
    print x

-- Let's try ST lazy without STRef
testFun1 :: (Monad m) => m [Int]
testFun1 = mapM (\x -> pure $ x * x) [1 ..]

testL, testS :: [Int]
testL = STL.runST testFun1
testS = STS.runST testFun1

-- take 5 testL
-- [1,4,9,16,25]
-- take 5 testS
-- Hangs
-- Conclusion: Lazy ST is lazy, problem is in STRef
-- And STRef lazy is implemented using strict STRef
-- Read: https://stackoverflow.com/a/24085928
