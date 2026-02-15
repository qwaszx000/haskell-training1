{-# LANGUAGE OverloadedStrings #-}

module Strings where

-- https://oleksii.shmalko.com/2015/haskell-string-types/
-- https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html
-- https://free.cofree.io/2020/05/06/string-types/
-- https://academy.fpblock.com/haskell/tutorial/string-types/
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/bytestring-bits-and-pieces

-- Standard string is [Char]
-- It uses linked list and is very inefficient

-- Data.Text uses an array of utf-16 units as a string representation
-- It also provides lazy realisation to avoid storing all data in memory
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL

-- Data.ByteString is an array of bytes
-- Used mainly for serialization/deserialization, networking, binary files
-- Data.Text.Encoding allows to transform Text(utf16) <-> ByteString
-- Default ByteString uses [Word8], but also [Char8] is available
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Data.Bits (xor)
import Data.Char (chr, ord)

testFilePath = "./test.txt"

testText :: String
testText =
    "Hello, привет мир\n\
    \And hello again"

testCmpConvert :: IO ()
testCmpConvert = do
    -- putStrLn testText
    print testText

    let ts = T.pack testText
    print ts

    -- This breaks utf16 symbols
    let bsc = BC.pack testText
    print bsc

    -- let bsw = B.pack testText
    -- print bsw
    return ()

testWriteFileT :: IO ()
testWriteFileT = do
    let ts = T.pack testText
    TIO.writeFile testFilePath ts

    -- readFile docs recommends using ByteString to read file, then convert it to required format
    -- It said that TIO functions depend on locale, and can be slow
    cont <- TIO.readFile testFilePath
    TIO.putStrLn cont

testWriteFileB :: IO ()
testWriteFileB = do
    -- convert it to Text first, to avoid breaking unicode
    let ts = T.pack testText

    -- Encode it as utf32BE(instead of utf-16 default for Text)
    let bcs = TE.encodeUtf32BE ts

    B.writeFile testFilePath bcs
    cont <- B.readFile testFilePath

    print cont
    let text = TE.decodeUtf32BE cont
    TIO.putStrLn text

    -- Simple xor
    let t = T.map (chr . (5 `xor`) . ord) text
    TIO.putStrLn t

    -- Same for bytestring
    let b = B.map (5 `xor`) cont
    print b

    TIO.putStrLn "hello тест"

    -- Broken utf8
    B.putStr "hello b тест\n"
    BC.putStrLn "hello bc тест"
