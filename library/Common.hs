{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common where

import Data.Aeson
import qualified Data.ByteString as B

import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import qualified Data.Text as Text
import Control.Monad(mzero)
import Web.HttpApiData (FromHttpApiData(..))
import qualified Data.ByteString.Char8 as C8

-- import GHC.Generics
-- import qualified Data.Aeson as Aeson
-- import Data.String(IsString)
import Debug.Trace
-- Make bytestrig type usable by api.
instance FromHttpApiData B.ByteString where
  parseUrlPiece = Right . encodeUtf8
  parseQueryParam = Right . encodeUtf8
  -- parseUrlPiece = Right . encodeUtf8
  -- parseQueryParam = Right . encodeUtf8

-- instance FromJSON B.ByteString where
--   parseJSON (String text) = return $ ((traceShow "Done") . encodeUtf8 . traceShowId) (text)
--   parseJSON _            = mzero

-- instance ToJSON B.ByteString where
--   toJSON = toJSON . decodeUtf8
