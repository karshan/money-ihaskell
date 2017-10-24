{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
module Util where

import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray as BA

import           Protolude hiding (hash, maybeToEither)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e

sha256 :: (StringConv a ByteString, StringConv ByteString b) => a -> b
sha256 x = toS $ (BA.convert :: Digest SHA256 -> ByteString) $ ((hash :: ByteString -> Digest SHA256) (toS x))
