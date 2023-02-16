{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (ByteString)
import Lib

main :: IO ()
main = do
    let secret = "string-with-exactly-32-character" :: ByteString
        msg = "Do you wanna dance and hold my hand?" :: ByteString
        key = encodeKey (undefined :: AES256) secret :: Key AES256 ByteString
        (iv, encrypted) = encode key msg :: (CryptoIV, EncryptedMessage)
        decrypted = decode key iv encrypted
    print (decrypted :: ByteString)
