module ContTest where

import Control.Monad (when)
import Control.Monad.Cont
import Control.Monad.Trans (liftIO)

-- https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-Cont.html#g:6
-- https://haskellforall.com/2012/12/the-continuation-monad
-- https://hackage-content.haskell.org/package/transformers-0.6.3.0/docs/Control-Monad-Trans-Cont.html
-- https://github.com/quchen/articles/blob/master/cont_monad.md
-- https://newartisans.com/2013/05/understanding-continuations/

askName :: Int -> IO (Either String String)
askName maxAttempts = evalContT $ do
    (askAgain, attempts) <- label maxAttempts
    callCC $ \fin -> do
        checkAttempts attempts fin
        res <- askNameIter (askAgain $ attempts - 1)
        fin res
        liftIO $ putStrLn "This will not be called"
        pure $ Left "And this will not be returned"
  where
    checkAttempts aleft fExit = do
        when (aleft == 0) (fExit $ Left "No more attempts left")

    askNameIter fRetry = do
        liftIO $ putStrLn "Enter your name:"
        name <- liftIO getLine
        if length name < 3
            then do
                liftIO $ putStrLn "Name is too short, try again"
                fRetry
            else pure $ Right name

main :: IO ()
main = do
    name <- askName 3
    putStrLn $ "Your name is " <> show name

-- ghci> :main
-- Enter your name:
-- 1
-- Name is too short, try again
-- Enter your name:
-- 2
-- Name is too short, try again
-- Enter your name:
-- 3
-- Name is too short, try again
-- Your name is Left "No more attempts left"
-- ghci> :main
-- Enter your name:
-- 1234
-- Your name is Right "1234"
