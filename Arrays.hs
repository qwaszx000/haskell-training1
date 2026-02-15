module Arrays where

import Data.Array.IO (IOArray)
import Data.Array.MArray (MArray, freeze, getAssocs, getElems, newArray_, writeArray)
import Data.Ix (Ix)

-- https://learnxbyexample.com/haskell/arrays/
-- https://wiki.haskell.org/Arrays
-- https://stackoverflow.com/questions/9611904/haskell-lists-arrays-vectors-sequences
-- https://github.com/haskell/vector
-- https://academy.fpblock.com/haskell/library/vector/
-- https://mmhaskell.com/data-structures/vector

main :: IO ()
main = do
    arr <- newArray_ (0, 9) :: IO (IOArray Int Int)

    fillArrayWith 5 arr

    els <- getElems arr
    print els

    return ()

fillArrayWith :: Int -> IOArray Int Int -> IO ()
fillArrayWith x arr = do
    assocs <- getAssocs arr
    mapM_ (place x) assocs
  where
    place :: Int -> (Int, e) -> IO ()
    place v (ind, _) = writeArray arr ind v
