{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tests where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Wai as W

import API
import Network.Wai (Application)
import Servant (serve)
import qualified Server as MS

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite (runMigration, withSqliteConn)

import Data.Aeson
import Data.Text
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = defaultMain tests

echoApp :: Application
{-# NOINLINE echoApp #-}
echoApp = unsafePerformIO $ runStderrLoggingT $ do
    let t :: Text = "hello 1"
    withSqliteConn
        "./tmp.sqlite3"
        ( \conn -> do
            MS.prepareDb conn
            pure $ MS.echoWaiApp (MS.Env t conn)
        )

-- main = do
--     serverT <- forkIO MS.main
--     threadDelay 1_000_000

--     killThread serverT

tests :: TestTree
tests = testGroup "Tests" [qced, unittests]

instance QC.Arbitrary EchoRec where
    arbitrary = do
        t1 <- QC.arbitrary @QC.UnicodeString
        t2 <- QC.arbitrary @QC.UnicodeString
        pure $ EchoRec (pack $ QC.getUnicodeString t1) (pack $ QC.getUnicodeString t2)

qced :: TestTree
qced =
    testGroup
        "QuickTest"
        [ QC.testProperty "Some test" $
            \(er :: EchoRec) -> case (decode . encode) er of
                Just x -> x == er
                Nothing -> True
        ]

unittests :: TestTree
unittests =
    testGroup
        "Unit tests"
        [ W.testWai echoApp "Check1" $ do
            res <- W.get "/echo/Test1/json"
            liftIO $ print res
        ]
