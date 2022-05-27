module Modules.Github.Main
  ( getContent
  ) where

import RIO
import qualified System.IO
import qualified Network.HTTP.Simple as HTTP
import qualified Modules.Github.Models.GetContentOptions.Main as GetContentOptions

getContent :: GetContentOptions.GetContentOptions -> System.IO.IO ByteString
getContent options = do
  endpoint <- HTTP.parseRequest "https://api.github.com"
  let path = "/repos/" <> options.repository <> "/contents/" <> options.path
      req  = endpoint
           & HTTP.setRequestPath (encodeUtf8 path)
           & HTTP.addToRequestQueryString [("ref", Just (encodeUtf8 options.sha))]
           & HTTP.addRequestHeader "User-Agent" options.userAgent
           & HTTP.addRequestHeader "Accept" "application/vnd.github.v3.raw"
  HTTP.getResponseBody <$> HTTP.httpBS req

