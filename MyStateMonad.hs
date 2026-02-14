module MyStateMonad where

import Control.Monad.Trans (MonadTrans, lift)
import Data.Functor.Identity (Identity, runIdentity)

type State s a = StateT s Identity a
newtype StateT s m a = StateT {runStateT :: s -> m (s, a)}

instance (Monad m) => Functor (StateT s m) where
    fmap f (StateT v) =
        StateT
            ( \state -> do
                (state', value) <- v state
                pure (state', f value)
            )

instance (Monad m) => Applicative (StateT s m) where
    pure v = StateT (\state -> pure (state, v))
    liftA2 f (StateT fa) (StateT fb) =
        StateT
            ( \state -> do
                (state', a) <- fa state
                (state'', b) <- fb state'
                pure (state'', f a b)
            )

instance (Monad m) => Monad (StateT s m) where
    (StateT fv) >>= f =
        StateT
            ( \state -> do
                (state', v) <- fv state
                runStateT (f v) state'
            )

instance MonadTrans (StateT s) where
    lift mv =
        StateT
            ( \state -> do
                v <- mv
                pure (state, v)
            )

-- Get state
get :: (Monad m) => StateT s m s
get = StateT (\state -> pure (state, state))

-- Set state
put :: (Monad m) => s -> StateT s m ()
put state = StateT (\_ -> pure (state, ()))

-- fmap state
modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = StateT (\state -> pure (f state, ()))

testState :: State Int Int
testState = do
    x1 <- get
    modify (+ 5)
    x2 <- get
    pure $ x1 + x2

testStateT :: StateT Int IO Int
testStateT = do
    x1 <- get
    lift $ putStrLn $ "x1: " ++ show x1
    modify (+ 5)
    x2 <- get
    lift $ putStrLn $ "x2: " ++ show x2
    pure $ x1 + x2

runTestState :: Int -> (Int, Int)
runTestState = runIdentity . runStateT testState

runTestStateT :: Int -> IO (Int, Int)
runTestStateT = runStateT testStateT

-- ghci> runTestState 2
-- (7,9)
-- ghci> runTestState 3
-- (8,11)
-- ghci> runTestStateT 3
-- x1: 3
-- x2: 8
-- (8,11)
