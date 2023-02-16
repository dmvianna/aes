{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib (
    encode,
    decode,
) where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.Random.Types qualified as CRT
import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

data Key c a where
    Key :: (BlockCipher c, ByteArray a) => a -> Key c a

newtype EncryptedMessage = EncryptedMessage {renderEncryptedMessage :: ByteString}
newtype CryptoIV = CryptoIV {renderCryptoIV :: ByteString}

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
    (BlockCipher c, ByteArray secret, ByteArray message) =>
    Key c secret ->
    message ->
    (CryptoIV, EncryptedMessage)
encode = undefined

decode ::
    (BlockCipher c, ByteArray message, ByteArray secret) =>
    Key c secret ->
    CryptoIV ->
    EncryptedMessage ->
    message
decode = undefined
