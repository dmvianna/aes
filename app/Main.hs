{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (ByteString)
import Lib (AES256, Key, decode, encode, encodeKey)

algo :: AES256
algo = undefined

main :: IO ()
main = do
    let secret = "string-with-exactly-32-character" :: ByteString
        msg = "Do you wanna dance and hold my hand?" :: ByteString
        key = encodeKey algo secret :: Key AES256 ByteString
    eEncode <- encode algo key msg
    eDecrypted <- case eEncode of
        Right (iv, encrypted) ->
            pure $ decode algo key iv encrypted
        Left e -> pure $ Left e
    case eDecrypted of
        Left e -> print e
        Right decrypted -> print decrypted
