module RefsIO where

import Control.Monad (forever)
import Data.IORef
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem
import System.Mem.Weak

-- https://stackoverflow.com/questions/6668716/haskell-lazy-bytestring-read-write-progress-function
-- https://stackoverflow.com/questions/24068399/haskell-performance-of-iorefs/24071781
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-IORef.html
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-Mem.html
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-Mem-Weak.html

testWeak :: IO Int
testWeak = do
    x <- newIORef 5
    wx <- mkWeakIORef x (print "X was GCd")
    -- forever $ writeIORef x 6 -- blocks, expectedly
    -- Also IO has no lazy variants
    unsafeInterleaveIO $ forever $ writeIORef x 6 -- way to fix
    writeIORef x 7
    xv <- readIORef x
    pure xv

testInterleave :: IO Int
testInterleave = do
    print "Interleave called"
    pure 55

infTest :: Int -> [Int] -> IO [Int]
infTest _ [] = pure []
infTest n (x : xs) = unsafeInterleaveIO $ do
    print $ "Element " ++ show n ++ "is accessed"
    rest <- infTest (n + 1) xs
    pure $ x : rest

main :: IO ()
main = do
    x <- testWeak
    performMajorGC -- x is collected and "X was GCd" is printed
    -- Only it doesn't block and this output can mixup with other stdout info
    -- If you remove it - you can still get this message in GHCi after :main finishes, also can be inside other chars
    print x

    y <- unsafeInterleaveIO testInterleave
    print "Here we are"
    -- "Interleave called" is printed here because of laziness created by unsafeInterleaveIO
    print y

    -- unsafeInterleaveIO can be a used to implement lazy actions, lazy strings
    -- Also https://hackage.haskell.org/package/bytestring-progress-1.0.1/docs/src/Data-ByteString-Lazy-Progress.html
    -- uses it
    -- Also maybe it can be used to track when lazy value is evaluated?
    l <- infTest 0 [1 ..]
    print $ take 5 l
    print $ l !! 8
    print $ take 10 l

-- ghci> :main
-- 7
-- "X was GCd"
-- ghci> :main
-- 7
-- "X waghci> s GCd"
