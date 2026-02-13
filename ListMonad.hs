module Main where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (MonadPlus)

main :: IO ()
main = do
    putStrLn "Hello"

moves :: [Int]
moves = [1, 2, 3]

-- listMonadTest [1,2,3]
-- [2,3,4,3,4,5,4,5,6]
listMonadTest :: [Int] -> [Int]
listMonadTest ns = do
    n <- ns
    n2 <- moves
    return (n + n2)

-- Also cool thing:
-- ghci> take 5 [x*x | x <- [1..], x > 5]
-- [36,49,64,81,100]

--- My realisation

data MyList a = MyList a (MyList a) | MyEmpty
    deriving (Show)

myJoin :: MyList (MyList a) -> MyList a
myJoin (MyList l ls) = myCat l $ myJoin ls
myJoin MyEmpty = MyEmpty

myCat :: MyList a -> MyList a -> MyList a
myCat MyEmpty l = l
myCat l MyEmpty = l
myCat (MyList l1 l1s) l2s = MyList l1 (myCat l1s l2s)

-- For REPL
-- a = MyList 5 MyEmpty
-- b = MyList 1 (MyList 2 (MyList 3 MyEmpty))
-- c = MyList b (MyList a MyEmpty)

-- Functor
-- fmap (+2) b

instance Functor MyList where
    fmap _ MyEmpty = MyEmpty
    fmap f (MyList a as) = MyList (f a) MyEmpty `myCat` fmap f as

-- Applicative
-- ghci> [(+1), (+2)] <*> [1,2,3]
-- [2,3,4,3,4,5]
-- My:
-- f = MyList (+1) (MyList (+2) MyEmpty)
-- ghci> f <*> b
-- MyList 2 (MyList 3 (MyList 4 (MyList 3 (MyList 4 (MyList 5 MyEmpty)))))

instance Applicative MyList where
    pure a = MyList a MyEmpty

    MyEmpty <*> _ = MyEmpty
    _ <*> MyEmpty = MyEmpty
    MyList f fs <*> as = fmap f as `myCat` (fs <*> as)

-- Monad
-- ghci> [1,2,3,4] >>= (\x -> [x+1])
-- [2,3,4,5]
-- ghci> [1,2,3,4] >>= (\x -> [x+1]) >>= (\x -> [x*2])
-- [4,6,8,10]
-- ghci> [1,2,3,4] >>= (\x -> []) >>= (\x -> [x*2])
-- []
-- ghci> [1,2,3,4] >>= (\x -> fail "1") >>= (\x -> [x*2]) -- fail is from MonadFail
-- []
-- ghci> [1,2,3,4] >>= (\x -> if x == 2 then [2] else []) >>= (\x -> [x*2])
-- [4]
-- My:
-- ghci> b >>= (\x -> MyList (x+1) MyEmpty)
-- MyList 2 (MyList 3 (MyList 4 MyEmpty))
-- ghci> b >>= (\x -> MyList (x+3) MyEmpty)
-- MyList 4 (MyList 5 (MyList 6 MyEmpty))
-- ghci> b >>= (\x -> MyList (x+3) MyEmpty) >>= (\x -> MyList (x*2) MyEmpty)
-- MyList 8 (MyList 10 (MyList 12 MyEmpty))
-- ghci> b >>= (\x -> MyEmpty) >>= (\x -> MyList (x*2) MyEmpty)
-- MyEmpty
-- ghci> b >>= (\x -> if x == 1 then MyList 1 MyEmpty else MyEmpty) >>= (\x -> MyList (x*2) MyEmpty)
-- MyList 2 MyEmpty

instance Monad MyList where
    MyEmpty >>= _ = MyEmpty
    MyList a as >>= f = f a `myCat` (as >>= f)

-- ghci> b >>= (\x -> fail "1") >>= (\x -> MyList (x*2) MyEmpty)
-- MyEmpty

instance MonadFail MyList where
    fail _ = MyEmpty

-- ghci> foldr (\x acc -> acc + x) 0 [1,2,3]
-- 6
-- ghci> foldr (\x acc -> acc + x) 0 b
-- 6

instance Foldable MyList where
    foldr _ acc MyEmpty = acc
    foldr facc acc (MyList a as) = foldr facc (facc a acc) as

-- ghci> [1,2,3] <> [4,5,6]
-- [1,2,3,4,5,6]
-- ghci> b <> b
-- MyList 1 (MyList 2 (MyList 3 (MyList 1 (MyList 2 (MyList 3 MyEmpty)))))

instance Semigroup (MyList a) where
    (<>) = myCat

-- ghci> traverse (\x -> Just x) [1,2,3]
-- Just [1,2,3]
-- ghci> traverse (\x -> if x == 2 then Nothing else Just x) [1,2,3]
-- Nothing
-- ghci> traverse (\x -> if x == 2 then Just 5 else Just x) [1,2,3]
-- Just [1,5,3]
-- ghci> traverse (\x -> if x == 2 then Just 5 else Just x) []
-- Just []
-- My:
-- ghci> traverse (\x -> Just x) b
-- Just (MyList 1 (MyList 2 (MyList 3 MyEmpty)))
-- ghci> traverse (\x -> if x == 2 then Nothing else Just x) b
-- Nothing
-- ghci> traverse (\x -> if x == 2 then Just 5 else Just x) b
-- Just (MyList 1 (MyList 5 (MyList 3 MyEmpty)))
-- ghci> traverse (\x -> if x == 2 then Just 5 else Just x) b
-- Just (MyList 1 (MyList 5 (MyList 3 MyEmpty)))
-- ghci> traverse (\x -> if x == 2 then Just 5 else Just x) MyEmpty
-- Just MyEmpty

instance Traversable MyList where
    traverse _ MyEmpty = pure MyEmpty
    traverse f (MyList a as) = liftA2 myCat fa fas
      where
        fa = fmap (\x -> MyList x MyEmpty) (f a)
        fas = traverse f as

-- ghci> mempty :: [Int]
-- []
-- ghci> mappend [1,2,3] [4]
-- [1,2,3,4]
-- My:
-- ghci> mempty :: MyList Int
-- MyEmpty
-- ghci> mappend b (MyList 4 MyEmpty)
-- MyList 1 (MyList 2 (MyList 3 (MyList 4 MyEmpty)))

instance Monoid (MyList a) where
    mempty = MyEmpty

-- ghci> [1,2,3] <|> [4,5,6]
-- [1,2,3,4,5,6]
-- My:
-- ghci> empty :: MyList Int
-- MyEmpty
-- ghci> a <|> b
-- MyList 5 (MyList 1 (MyList 2 (MyList 3 MyEmpty)))

instance Alternative MyList where
    empty = MyEmpty
    ls <|> rs = ls `myCat` rs

-- ghci> mzero :: [Int]
-- []
-- ghci> mplus [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
-- My:
-- ghci> mzero :: MyList Int
-- MyEmpty
-- ghci> mplus a b
-- MyList 5 (MyList 1 (MyList 2 (MyList 3 MyEmpty)))

instance MonadPlus MyList
