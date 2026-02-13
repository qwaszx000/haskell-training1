module MyMaybeMonad where

import Control.Monad.Trans (MonadTrans, lift)
import Text.Read (readMaybe)

data MyMaybe a = Some a | None
    deriving (Show, Eq)

instance Functor MyMaybe where
    fmap _ None = None
    fmap f (Some x) = Some $ f x

instance Applicative MyMaybe where
    pure x = Some x
    None <*> _ = None
    _ <*> None = None
    (Some f) <*> (Some x) = Some (f x)

instance Monad MyMaybe where
    None >>= _ = None
    (Some x) >>= f = f x

someTestFunc :: Int -> MyMaybe Int
someTestFunc 0 = None
someTestFunc n = Some n

testMonadDoNone :: MyMaybe Int
testMonadDoNone = do
    Some 5
    x <- someTestFunc 6
    None
    return 7

testMonadDoSome :: MyMaybe Int
testMonadDoSome = do
    y <- someTestFunc 5 -- 0 makes it return None
    Some y

testMonadDoPure :: MyMaybe Int
testMonadDoPure = do
    Some 5
    pure 6

-- Transformer

newtype MyMaybeT m a = MyMaybeT {runMyMaybeT :: m (MyMaybe a)}

instance (Monad m) => Functor (MyMaybeT m) where
    fmap f (MyMaybeT inner) = MyMaybeT $ do
        x <- inner
        pure $ case x of
            None -> None
            Some a -> Some $ f a

instance (Monad m) => Applicative (MyMaybeT m) where
    pure = MyMaybeT . pure . Some
    (MyMaybeT innerF) <*> (MyMaybeT innerV) = MyMaybeT $ do
        f <- innerF
        v <- innerV
        pure $ f <*> v

instance (Monad m) => Monad (MyMaybeT m) where
    (MyMaybeT innerM) >>= f = MyMaybeT $ do
        v <- innerM
        case v of
            None -> pure None
            Some a -> runMyMaybeT $ f a

instance MonadTrans MyMaybeT where
    lift = MyMaybeT . fmap pure

liftMaybe :: (Monad m) => Maybe a -> MyMaybeT m a
liftMaybe Nothing = MyMaybeT $ pure None
liftMaybe (Just x) = MyMaybeT $ pure $ Some x

testMyTrans :: MyMaybeT IO Int
testMyTrans = do
    v <- lift $ getLine
    x <- liftMaybe $ readMaybe v
    v <- lift $ getLine
    y <- liftMaybe $ readMaybe v
    return $ x + y

runTestTrans :: IO ()
runTestTrans = do
    x <- runMyMaybeT testMyTrans
    print x

-- runTestTrans
-- 123
-- 321
-- Some 444
-- runTestTrans
-- 123
-- ggg
-- None
-- runTestTrans
-- ggg
-- None
