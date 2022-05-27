module Modules.Docker.Models.LogGetResponse.Main
  ( LogGetResponse(..)
  , fromResponse
  ) where

import RIO
import qualified Network.HTTP.Simple as HTTP

data LogGetResponse = LogGetResponse
  { raw :: ByteString
  }
    deriving (Eq, Show)

fromResponse :: HTTP.Response ByteString -> LogGetResponse
fromResponse = LogGetResponse . HTTP.getResponseBody
