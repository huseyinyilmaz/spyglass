{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common where

import Data.Aeson
import qualified Data.ByteString as B
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Control.Monad(mzero)
import Web.HttpApiData (FromHttpApiData(..))

-- Make bytestrig type usable by api.
instance FromHttpApiData B.ByteString where
  parseUrlPiece = Right . encodeUtf8
  parseQueryParam = Right . encodeUtf8

instance FromJSON B.ByteString where
  parseJSON (String str) = return $ encodeUtf8 str
  parseJSON _            = mzero

instance ToJSON B.ByteString where
  toJSON = (toJSON . decodeUtf8)
