-- For last variant
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module TaglessFinal where

-- https://jproyo.github.io/posts/2019-03-17-tagless-final-haskell/
-- https://serokell.io/blog/introduction-tagless-final
--
-- https://stackoverflow.com/questions/75840380/avoid-boilerpate-of-tagless-final-in-haskell
-- https://www.reddit.com/r/haskell/comments/nmj8hz/final_tagless_encodings_have_little_to_do_with/
-- https://www.foxhound.systems/blog/final-tagless/
-- https://www.reddit.com/r/haskell/comments/a40z4t/blog_introduction_to_tagless_final/
-- https://peddie.github.io/encodings/encodings-text.html
-- https://ro-che.info/articles/2016-02-03-finally-tagless-boilerplate

import Control.Monad.State
import qualified Data.Map as Map

type UserId = Int

data UserData = UserData
    { uId :: UserId
    , uName :: String
    , uStars :: Int
    }
    deriving (Show)

type UsersMap = Map.Map UserId UserData

class (Monad m) => UsersDB m where
    getUserById :: UserId -> m (Maybe UserData)
    updateUser :: UserData -> m ()

class (Monad m) => Logger m where
    logStr :: String -> m ()

performAction :: (UsersDB m, Logger m) => m ()
performAction = do
    user1 <- getUserById 1
    case user1 of
        Nothing -> logStr "User not found"
        Just u -> do
            let u' = u{uId = 2, uStars = uStars u + 1}
            logStr "Inc stars"
            updateUser u'

--- First interpreter
newtype MyApp a = MyApp {unMyApp :: StateT UsersMap IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState UsersMap)

runMyApp :: UsersMap -> MyApp a -> IO (a, UsersMap)
runMyApp env app = runStateT (unMyApp app) env

instance UsersDB MyApp where
    getUserById id = do
        uMap <- get
        pure $ Map.lookup id uMap

    updateUser uData = do
        uMap <- get
        let uMap' = Map.insert (uId uData) uData uMap
        put uMap'

instance Logger MyApp where
    logStr = liftIO . putStrLn

--- Second interpreter
newtype PrintyApp a = PrintyApp {unPrintyApp :: IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance UsersDB PrintyApp where
    getUserById uid = do
        logStr $ "Accessing user " ++ show uid
        pure Nothing
    updateUser udata = do
        logStr $ "Updating user " ++ show udata
        pure ()

instance Logger PrintyApp where
    logStr = liftIO . putStrLn . ("Printy: " ++)

runPrintyApp :: PrintyApp a -> IO a
runPrintyApp = unPrintyApp

--- Modular way
-- https://chrispenner.ca/posts/mock-effects-with-data-kinds

data DBImpl = DBInMem | DBErr
data LoggerImpl = LoggerPrinty | LoggerIgnore

newtype ModularTest (db :: DBImpl) (logger :: LoggerImpl) a = ModularTest {unModTest :: StateT UsersMap IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadState UsersMap)

runModTest :: UsersMap -> ModularTest db logger a -> IO (a, UsersMap)
runModTest env app = runStateT (unModTest app) env

instance UsersDB (ModularTest DBInMem logger) where
    getUserById id = do
        uMap <- get
        pure $ Map.lookup id uMap

    updateUser uData = do
        uMap <- get
        let uMap' = Map.insert (uId uData) uData uMap
        put uMap'

instance UsersDB (ModularTest DBErr logger) where
    getUserById uid = do
        liftIO $ putStrLn $ "Accessing user " ++ show uid
        pure Nothing
    updateUser udata = do
        liftIO $ putStrLn $ "Updating user " ++ show udata
        -- Can't use it here unfortunately, compiler is not sure we have Logger instance here
        -- Don't know why
        -- logStr $ "Updating user " ++ show udata
        pure ()

instance Logger (ModularTest db LoggerPrinty) where
    logStr = liftIO . putStrLn . ("Printy: " ++)

instance Logger (ModularTest db LoggerIgnore) where
    logStr _ = do
        liftIO $ putStrLn "Ignored log"
        pure ()

---

main :: IO ()
main = do
    let stateMap = Map.singleton 1 $ UserData 1 "Name1" 0
    (_, state') <- runMyApp stateMap performAction
    print state'

    runPrintyApp performAction

    -- And here we can use different combinations
    (_, state2) <- runModTest @DBErr @LoggerIgnore stateMap performAction
    print state2
