{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Base64.Types (extractBase64)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Base64

testStr :: BL.ByteString
testStr = "Hello world"

main :: IO ()
main = do
    print testStr

    let enc = encodeBase64' testStr
    print enc
    let enc_raw = extractBase64 enc
    print enc_raw
