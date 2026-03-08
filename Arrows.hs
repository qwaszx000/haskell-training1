{-# LANGUAGE Arrows #-}

module Arrows where

import Control.Arrow
import qualified Control.Category as Cat
import Data.Traversable (mapAccumL)

-- https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial
-- https://en.wikibooks.org/wiki/Haskell/Understanding_arrows

newtype Circuit s a = Circuit {unCircuit :: s -> (Circuit s a, a)}

instance Cat.Category Circuit where
    id = Circuit $ \s -> (Cat.id, s)
    (Circuit c1) . (Circuit c2) = Circuit $
        \x ->
            let
                (c1', r1') = c2 x
                (c2', r2') = c1 r1'
             in
                (c2' Cat.. c1', r2')

instance Arrow Circuit where
    arr :: (b -> c) -> Circuit b c
    arr f = Circuit $ \s -> (arr f, f s)
    first :: Circuit b c -> Circuit (b, d) (c, d)
    first (Circuit c1) = Circuit $ \(sb, sd) ->
        let
            (c1', sb') = c1 sb
         in
            (first c1', (sb', sd))

runCircuit :: Circuit s a -> [s] -> [a]
-- runCircuit _ [] = []
-- runCircuit (Circuit c) (x : xs) =
--     let
--         (c', r) = c x
--      in
--         r : runCircuit c' xs
runCircuit cir = snd . mapAccumL (\(Circuit f) s -> f s) cir

accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \x ->
    let
        (res, acc') = f x acc
     in
        (accum acc' f, res)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc $ \a b -> let b' = f a b in (b', b')

total :: (Num a) => Circuit a a
total = accum' 0 (+)

-- ghci> runCircuit total [1, 2, 3, 4, 5]
-- [1,3,6,10,15]
-- ghci> runCircuit total [1, 2, 3, 4, 0, 5]
-- [1,3,6,10,10,15]
-- ghci> runCircuit total [1, 2, 0, 0, 3, 4, 0, 5]
-- [1,3,3,3,6,10,10,15]

mean1 :: (Fractional a) => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

-- ghci> runCircuit mean1 [1, 2, 0, 0, 3, 4, 0, 5]
-- [1.0,1.5,1.0,0.75,1.2,1.6666666666666667,1.4285714285714286,1.875]

mean2 :: (Fractional a) => Circuit a a
mean2 = proc value -> do
    t <- total -< value
    c <- total -< 1
    returnA -< (t / c)

-- ghci> runCircuit mean2 [1, 2, 0, 0, 3, 4, 0, 5]
-- [1.0,1.5,1.0,0.75,1.2,1.6666666666666667,1.4285714285714286,1.875]

testKIO1 :: IO ()
testKIO1 = do
    let readl = Kleisli $ const getLine
    let putl = Kleisli print
    let putm = ("aaa" ++) ^>> (++ "bbb") ^>> Kleisli putStrLn
    let k = readl >>> (putl &&& putm) >>^ uncurry const
    runKleisli k ()

testKIO2 :: IO ()
testKIO2 = do
    let readl = Kleisli $ const getLine
    let putl = ("First: " ++)
    let putm = ("aaa" ++) ^>> (++ "bbb")
    let k = (readl >>> arr (putl &&& putm) >>^ (\(a, b) -> a ++ "\n" ++ b)) >>> Kleisli putStrLn
    runKleisli k ()

--- Test synchroneousness
-- How?

newtype SF a b = SF {unSF :: [a] -> [b]}

instance Cat.Category SF where
    id :: SF a a
    id = SF id
    (.) :: SF b c -> SF a b -> SF a c
    (SF f) . (SF g) = SF $ f . g

instance Arrow SF where
    arr :: (b -> c) -> SF b c
    arr = SF . map

    -- (***) :: SF b c -> SF b' c' -> SF (b, b') (c, c')
    -- (SF f) *** (SF g) = SF $ unzip >>> first f >>> second g >>> uncurry zip

    first :: SF b c -> SF (b, d) (c, d)
    first (SF f) = SF $ unzip >>> first f >>> uncurry zip

runSF :: SF a b -> [a] -> [b]
runSF (SF f) = f

testSF1 :: IO ()
testSF1 = do
    let a = [1, 2, 3, 4, 5]
    let k = arr (5 +) >>^ (2 *) >>> (arr (5 +) &&& arr (+ 5)) >>^ uncurry (+)
    let ar = runSF k a
    print ar
