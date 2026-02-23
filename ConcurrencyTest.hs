module ConcurrencyTest where

import Control.Concurrent
import Control.Exception
import Control.Monad (replicateM)
import Data.IORef

import Control.Concurrent.STM
import Control.Monad.STM

proveBadBehave :: IO ()
proveBadBehave = do
    a <- newIORef 5
    acts <- replicateM 4 (forkIO $ doSomeThing a)
    av <- readIORef a
    print av
    -- Fails sometimes, but mostly we don't get correct value so it's good enough
    assert (av /= 25) $ pure ()
  where
    doSomeThing :: IORef Int -> IO ()
    doSomeThing v = modifyIORef v (5 +)

fix1way :: IO ()
fix1way = do
    a <- newIORef 5
    acts <- replicateM 4 (forkIO $ doSomeThing a)
    -- Bad fix obviously, but we can't wait for children to finish
    threadDelay 50
    av <- readIORef a
    print av
    assert (av == 25) $ pure ()
  where
    doSomeThing :: IORef Int -> IO ()
    doSomeThing v = atomicModifyIORef' v (\n -> (5 + n, ()))

fix2way :: IO ()
fix2way = do
    let tnum = 4
    a <- newIORef 5
    sem <- newQSemN tnum
    acts <- replicateM tnum (forkIO $ doSomeThing sem a)
    yield
    waitQSemN sem tnum
    av <- readIORef a
    print av
    assert (av == 5 + 5 * tnum) $ pure ()
  where
    doSomeThing :: QSemN -> IORef Int -> IO ()
    doSomeThing sem v = do
        waitQSemN sem 1
        atomicModifyIORef' v (\n -> (5 + n, ()))
        signalQSemN sem 1

fix3way :: IO ()
fix3way = do
    let tnum = 4
    let a = 5
    ch :: Chan Int <- newChan
    writeChan ch a
    sem <- newQSemN tnum
    acts <- replicateM tnum (forkIO $ doSomeThing ch)
    -- Also bad
    threadDelay 50
    av <- readChan ch
    print av
    assert (av == 5 + 5 * tnum) $ pure ()
  where
    doSomeThing :: Chan Int -> IO ()
    doSomeThing chan = do
        val <- readChan chan
        let val' = val + 5
        writeChan chan val'

-- Ok, let's try dining philosophers
-- https://en.wikipedia.org/wiki/Dining_philosophers_problem

pThink :: Int -> Chan String -> IO ()
pThink n logger = do
    writeChan logger $ "Philosopher " <> show n <> " thinks"
    threadDelay 100

createPs :: (a -> Int -> Chan String -> IO ()) -> [a] -> Int -> Chan String -> IO [ThreadId]
createPs _ _ 0 _ = pure []
createPs f (fv : fvs) n logger = do
    tid <- forkIO $ f fv n logger
    others <- createPs f fvs (n - 1) logger
    pure $ tid : others

initProblem :: IO (Int, Chan String, ThreadId)
initProblem = do
    ch <- newChan
    tid <- forkIO $ logger ch
    pure (5, ch, tid)
  where
    logger :: Chan String -> IO ()
    logger ch = do
        line <- readChan ch
        putStrLn line
        logger ch

splitForks :: Int -> [a] -> [(a, a)]
splitForks 0 _ = []
splitForks n forks =
    let
        [f1, f2] = take 2 forks
        forks' = f2 : (drop 2 forks ++ [f1])
     in
        (f1, f2) : splitForks (n - 1) forks'

newtype MyExcept = MyExcept String
    deriving (Show)

instance Exception MyExcept

dpMutex :: IO ()
dpMutex = do
    (nPs, logger, loggerId) <- initProblem
    forks <- initMutexes nPs
    let pairs = splitForks nPs forks
    pThreads <- createPs exceptionWrapper pairs nPs logger
    threadDelay 5000
    mapM_ (flip throwTo $ MyExcept "Hello") pThreads
    threadDelay 5000
    mapM_ killThread pThreads
  where
    initMutexes :: Int -> IO [MVar ()]
    initMutexes 0 = pure []
    initMutexes n = do
        v <- newMVar ()
        rest <- initMutexes (n - 1)
        pure $ v : rest

    takeFork :: MVar () -> IO ()
    takeFork = takeMVar

    tryTakeFork :: MVar () -> IO (Maybe ())
    tryTakeFork = tryTakeMVar

    releaseFork :: MVar () -> IO ()
    releaseFork m = putMVar m ()

    exceptionWrapper :: (MVar (), MVar ()) -> Int -> Chan String -> IO ()
    exceptionWrapper state pid logger = do
        philLogic state pid logger
            `catch` ( \(e :: MyExcept) -> do
                        writeChan logger $ "Philosopher " <> show pid <> " received exception: " <> show e
                        philLogic state pid logger
                    )

    philLogic :: (MVar (), MVar ()) -> Int -> Chan String -> IO ()
    philLogic state@(lf, rf) pid logger = do
        pThink pid logger
        takeFork lf
        writeChan logger $ "Philosopher " <> show pid <> " took left fork"
        rfAttempt <- tryTakeFork rf
        case rfAttempt of
            Nothing -> do
                writeChan logger $ "Philosopher " <> show pid <> " released left fork"
                releaseFork lf
            Just _ -> do
                writeChan logger $ "Philosopher " <> show pid <> " took right fork and eats"
                threadDelay 50
                releaseFork rf
                writeChan logger $ "Philosopher " <> show pid <> " released right fork"
                releaseFork lf
                writeChan logger $ "Philosopher " <> show pid <> " released left fork"
        philLogic state pid logger

dpSTM :: IO ()
dpSTM = do
    (nPs, logger, loggerId) <- initProblem
    forks <- initTVars nPs
    let pairs = splitForks nPs forks
    pThreads <- createPs philLogic pairs nPs logger
    threadDelay 10000
    mapM_ killThread pThreads
  where
    initTVars :: Int -> IO [TMVar ()]
    initTVars 0 = pure []
    initTVars n = do
        v <- newTMVarIO ()
        rest <- initTVars (n - 1)
        pure $ v : rest

    takeFork :: TMVar () -> STM ()
    takeFork = takeTMVar

    releaseFork :: TMVar () -> STM ()
    releaseFork t = putTMVar t ()

    philLogic :: (TMVar (), TMVar ()) -> Int -> Chan String -> IO ()
    philLogic state@(lf, rf) pid logger = do
        pThink pid logger
        atomically $ do
            takeFork lf
            takeFork rf
        writeChan logger $ "Philosopher " <> show pid <> " took both forks and eats"
        threadDelay 50
        atomically $ do
            releaseFork lf
            releaseFork rf
        philLogic state pid logger
