module TypeClassInduction where

import Data.Bifunctor (first)

-- Inspired by
-- https://quentinduval.github.io/blog/2017/02/03/code-quick-check-1.html
-- https://deque.blog/2017/02/03/code-your-own-quickcheck/

type TestResult = Either String String

class Generatable a where
    generate :: Int -> a

instance Generatable Bool where
    generate = even

instance Generatable Int where
    generate = id

instance Generatable String where
    generate = show

class Testable a where
    runTest :: a -> TestResult

instance Testable Bool where
    runTest b = case b of
        True -> Right "Success"
        False -> Left "Fail"

instance (Testable t, Generatable a) => Testable (a -> t) where
    runTest f = runNTests f 10
      where
        runNTests :: (a -> t) -> Int -> TestResult
        runNTests _ 0 = Right "Success(all attempts exhaused)"
        runNTests f n = do
            let res = runTest $ f $ generate n
            first (enrich n) res
            runNTests f (n - 1)

        enrich :: Int -> String -> String
        enrich n str = str ++ "(with arg=" ++ show n ++ ")"

-- Instance works by induction here
-- We only defined base case(Testable Bool) and induction "step" - Testable (a -> Testable)
myTestFunc :: Int -> Int -> Int -> String -> Bool
myTestFunc _ _ n str = n == read str

main :: IO ()
main = do
    let res = runTest myTestFunc
    print res

-- ghci> runTest ((==) :: String -> String -> Bool)
-- Left "Fail(with arg=9)(with arg=10)"
-- ghci> runTest ((/=) :: String -> String -> Bool)
-- Left "Fail(with arg=10)(with arg=10)"
