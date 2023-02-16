{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Test.Hspec (Spec, describe, hspec, it, runIO, shouldBe)

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    checkCripto

checkCripto :: Spec
checkCripto = undefined
