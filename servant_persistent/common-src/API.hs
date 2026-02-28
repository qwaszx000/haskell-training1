{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Aeson
import Servant.API

import Data.Proxy (Proxy (Proxy))
import Data.Text
import GHC.Generics

data EchoRec = EchoRec Text Text
    deriving (Show, Generic, Eq)

instance ToJSON EchoRec where
    toJSON (EchoRec t1 t2) = object ["t1" .= t1, "t2" .= t2]

instance FromJSON EchoRec where
    parseJSON = withObject "EchoRec" $ \v ->
        EchoRec <$> v .: "t1" <*> v .: "t2"

type EchoAPI =
    "echo" :> Capture "t1" Text :> QueryParam "t2" Text :> "raw" :> Get '[PlainText] Text
        :<|> "echo" :> Capture "t1" Text :> QueryParam "t2" Text :> "json" :> Get '[JSON] [EchoRec]
        :<|> "echo" :> Capture "t1" Text :> QueryParam "t2" Text :> "error" :> Get '[JSON] [EchoRec]

echoApi :: Proxy EchoAPI
echoApi = Proxy
