{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
    encode,
    decode,
    encodeKey,
    AES256,
    CryptoIV (CryptoIV),
    EncryptedMessage (EncryptedMessage),
    Key (Key),
) where

import Control.Exception (Exception)
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.Random.Types qualified as CRT
import Data.Bifunctor (Bifunctor (second))
import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

data Key c a where
    Key :: (BlockCipher c, ByteArray a) => a -> Key c a

newtype EncryptedMessage = EncryptedMessage {renderEncryptedMessage :: ByteString}
    deriving newtype (Ord, Eq, Monoid, Semigroup, ByteArray, ByteArrayAccess)

newtype CryptoIV = CryptoIV {renderCryptoIV :: ByteString}
    deriving newtype (Ord, Eq, Monoid, Semigroup, ByteArray, ByteArrayAccess)

encodeKey ::
    forall c a.
    (BlockCipher c, ByteArray a) =>
    c ->
    ByteString ->
    Key c a
encodeKey _ = Key . BA.pack . BS.unpack

-- | Generate a random initialization vector for a given block cipher
genRandomIV ::
    forall m c.
    (CRT.MonadRandom m, BlockCipher c) =>
    c ->
    m (Maybe (IV c))
genRandomIV _ = do
    bytes :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: c)
    return $ makeIV bytes

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
    CryptoFailed e -> Left e
    CryptoPassed a -> Right a

serialiseIV ::
    forall c.
    BlockCipher c =>
    IV c ->
    ByteString
serialiseIV = BA.convert

deserialiseIV ::
    (ByteArrayAccess b, BlockCipher c) =>
    b ->
    Maybe (IV c)
deserialiseIV = makeIV

encode ::
    (BlockCipher c, ByteArray a, ByteArray message, ByteArray encrypted) =>
    c ->
    Key c a ->
    message ->
    IO (Either AESError (CryptoIV, encrypted))
encode c key msg = do
    mIv <- genRandomIV c
    case mIv of
        Just iv -> do
            case transcode iv key msg of
                Left e -> pure $ Left e
                Right (civ, msg') -> pure $ Right (civ, msg')
        Nothing -> pure . Left $ AESErrorIV

transcode ::
    (BlockCipher c, ByteArray message, ByteArray secret, ByteArray a) =>
    IV c ->
    Key c a ->
    message ->
    Either AESError (CryptoIV, secret)
transcode iv key msg =
    case initCipher key of
        Right cipher ->
            Right
                ( CryptoIV . serialiseIV $ iv
                , ctrCombine cipher iv (BA.convert msg)
                )
        Left e -> Left $ AESErrorCrypto e

decode ::
    (BlockCipher c) =>
    c ->
    Key c ByteString ->
    CryptoIV ->
    EncryptedMessage ->
    Either AESError ByteString
decode _ key civ msg =
    let mIV = deserialiseIV civ.renderCryptoIV
     in case mIV of
            Nothing -> Left AESErrorIV
            Just iv -> case transcode iv key msg of
                Left e -> Left e
                Right (_, msg') -> Right msg'

data AESError
    = AESErrorCrypto CryptoError
    | AESErrorIV
    deriving (Show)

instance Exception AESError
