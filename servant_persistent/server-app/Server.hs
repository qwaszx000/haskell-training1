{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Server where

import Data.Text
import Network.Wai.Handler.Warp (run)
import Servant

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text.IO (putStrLn)

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

import Control.Monad.Except (ExceptT (..))
import Control.Monad.Logger (MonadLoggerIO, runStderrLoggingT)

import Prelude hiding (putStrLn)

import Control.Exception (throw, try)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import DB
import Database.Persist
import Database.Persist.Sqlite

import API

data Env = Env Text SqlBackend

newtype App a = App {runApp :: ReaderT Env IO a}
    deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader Env, MonadUnliftIO)

class WithDb m where
    runDb :: ReaderT SqlBackend m a -> m a

instance WithDb App where
    runDb act = do
        conn :: SqlBackend <- asks projectBackend
        runSqlConn act conn

instance BackendCompatible SqlBackend Env where
    projectBackend (Env _ sql) = sql

echoServer :: ServerT EchoAPI App
echoServer = echoRaw :<|> echoJson :<|> echoErr
  where
    echoRaw :: Text -> Maybe Text -> App Text
    echoRaw t1 mt2 = do
        let t2 = fromMaybe "" mt2
        liftIO $ putStrLn $ "Received raw req: " <> t1 <> " " <> pack (show mt2)
        k <- runDb $ insert $ Log t1 t2
        liftIO $ print k
        pure $ "Hello there: " <> t1 <> "|" <> t2

    echoJson :: Text -> Maybe Text -> App [EchoRec]
    echoJson t1 mt2 = do
        let t2 = fromMaybe "" mt2
        liftIO $ putStrLn $ "Received json req: " <> t1 <> " " <> pack (show mt2)
        rec :: Maybe (Entity Log) <- runDb $ selectFirst @SqlBackend [] []
        let addit = maybe [] (\e -> let r = entityVal e in [EchoRec (logT1 r) (logT2 r)]) rec
        pure $ [EchoRec t1 t2, EchoRec t2 t1] ++ addit

    echoErr :: Text -> Maybe Text -> App [EchoRec]
    echoErr t1 mt2 = do
        let t2 = fromMaybe "" mt2
        liftIO $ putStrLn $ "Received err req: " <> t1 <> " " <> pack (show mt2)

        throw $ err500{errBody = ("Error happened: " <> (BS.fromStrict $ TE.encodeUtf8 t1))}

        pure [EchoRec t1 t2, EchoRec t2 t1]

startEchoServer :: Env -> Server EchoAPI
startEchoServer env = hoistServer echoApi (prepApp env) echoServer
  where
    prepApp :: Env -> App a -> Handler a
    prepApp env app = Handler $ ExceptT $ try $ runReaderT (runApp app) env

echoWaiApp :: Env -> Application
echoWaiApp = serve echoApi . startEchoServer

prepareDb :: (MonadLoggerIO m, MonadUnliftIO m) => SqlBackend -> m ()
prepareDb = runReaderT (runMigration migrateAll)

main :: IO ()
main = runStderrLoggingT $ do
    let t :: Text = "hello 1"
    withSqliteConn
        "./tmp.sqlite3"
        ( \conn -> do
            prepareDb conn
            liftIO $ run 8081 $ echoWaiApp (Env t conn)
        )

-- http://127.0.0.1:8081/echo/test/json?t2=hey
-- http://127.0.0.1:8081/echo/test/raw?t2=hey
-- http://127.0.0.1:8081/echo/test/error?t2=hey
