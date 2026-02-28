{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import API

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text

import Data.Aeson
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client

echoRaw :: Text -> Maybe Text -> ClientM Text
echoJson :: Text -> Maybe Text -> ClientM [EchoRec]
echoErr :: Text -> Maybe Text -> ClientM [EchoRec]
(echoRaw :<|> echoJson :<|> echoErr) = client echoApi --

main :: IO ()
main = do
    httpManager <- newManager defaultManagerSettings
    let env = mkClientEnv httpManager (BaseUrl Http "127.0.0.1" 8081 "")
    res <- runClientM clientMain env
    case res of
        Left err -> putStrLn $ "Error: " <> show err
        Right _ -> pure ()

clientMain :: ClientM ()
clientMain = do
    liftIO $ print "Hello"

    resp1 <- echoRaw "Hello1" (Just "Hi1")
    liftIO $ print resp1

    resp2 <- echoJson "Hello2" (Just "Hi2")
    liftIO $ print resp2

    resp3 <- echoErr "Hello3" (Just "Hi3")
    liftIO $ print resp3
    liftIO $ print "Must not be printed"
