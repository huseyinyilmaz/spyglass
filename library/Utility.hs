module Utility where

import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
-- import Control.Monad.IO.Class (MonadIO)
import Network.Wai

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List (sortBy)
import Network.HTTP.Types.Status(created201,
                                 badRequest400,
                                 notFound404,
                                 methodNotAllowed405)
import qualified Network.HTTP as HTTP
import Network.URI ( parseURI )

import Data.Monoid((<>))
import Network.Wreq(get, responseBody)
import Network.HTTP.Client(HttpException(..))
import Control.Lens
import Control.Exception as E

-- import Network.HTTP.Client(Response)

toLower :: B.ByteString -> B.ByteString
toLower = encodeUtf8 . Text.toLower . decodeUtf8

reverseSort:: Ord a => [a] -> [a]
reverseSort = sortBy (flip compare)

noContent :: Response
noContent = do
  responseLBS created201 [] ""

notAllowed :: Response
notAllowed = do
  responseLBS methodNotAllowed405 [] ""


errorResponse :: LB.ByteString -> Response
errorResponse e = do
  responseLBS badRequest400 [] e

notFoundResponse :: LB.ByteString -> Response
notFoundResponse e = do
  responseLBS notFound404 [] e


getTitle::String
getTitle = foldr1 (<>) [
  "                               __                \n",
  "       _________  __  ______ _/ /___ ___________ \n",
  "      / ___/ __ \\/ / / / __ `/ / __ `/ ___/ ___/\n",
  "     (__  ) /_/ / /_/ / /_/ / / /_/ (__  |__  )  \n",
  "    /____/ .___/\\__, /\\__, /_/\\__,_/____/____/\n ",
  "       /_/    /____//____/                      \n"]

getLazyRequest
    :: String                   -- ^URL to fetch
    -> Maybe (HTTP.Request LB.ByteString)  -- ^The constructed request
getLazyRequest urlString =
  case parseURI urlString of
    Nothing -> Nothing
    Just u  -> Just $ HTTP.mkRequest HTTP.GET u

-- /search => search
-- /search/one/two => search/one/two
-- /search/one/two/ => search/one/two
buildPath :: [Text.Text] -> Text.Text
buildPath ps = Text.intercalate "/" (filter (/="") ps)

httpGet :: String -> IO (Maybe LB.ByteString)
httpGet url = do
  maybeResponse <- (Just <$> (get url)) `E.catch` exceptionHandler
  case maybeResponse of
    (Just r) -> do
      let body = r ^. responseBody
      return $ Just body
    Nothing -> return Nothing
  where
    exceptionHandler :: HttpException -> IO (Maybe a)
    exceptionHandler e = do
      putStrLn ("Could not get url " <> url <> " " <> (show e))
      return Nothing
