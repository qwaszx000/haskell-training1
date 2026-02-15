{-# LANGUAGE GADTs #-}

module FreeMonad where

-- https://deque.blog/2017/07/06/hexagonal-architecture-a-less-declarative-free-monad/
-- https://softwarepatternslexicon.com/functional/advanced-patterns/functional-abstractions/free-monad/
-- https://haskellforall.com/2012/06/you-could-have-invented-free-monads
-- https://haskellforall.com/2012/07/purify-code-using-free-monads

-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt.html
data Expr a where
    Print :: String -> Expr ()
    GetLine :: Expr String
    Add :: Int -> Int -> Expr Int
    Pure :: a -> Expr a
    Bind :: Expr a -> (a -> Expr b) -> Expr b

instance Functor Expr where
    fmap f ex = Bind ex (pure . f)

instance Applicative Expr where
    pure = Pure
    af <*> av = af >>= (`fmap` av)

instance Monad Expr where
    (>>=) = Bind

eval :: Expr a -> IO a
eval (Print str) = putStrLn str
eval GetLine = getLine
eval (Add a b) = pure $ a + b
eval (Pure v) = pure v
eval (Bind exp f) = do
    v <- eval exp
    eval $ f v

-- Here we can't do any IO besides allowed by our Expr monad
mainLogic :: Expr Int
mainLogic = do
    Print "test1"
    inp <- GetLine
    Print $ "You wrote: " ++ inp
    x <- Add 1 5
    Pure x

main :: IO ()
main = do
    n <- eval mainLogic
    print n
