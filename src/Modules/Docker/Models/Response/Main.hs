module Modules.Docker.Models.Response.Main
  ( parse
  ) where

import RIO
import qualified System.IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP

parse :: (Aeson.Value -> Aeson.Types.Parser a) -> HTTP.Response ByteString -> System.IO.IO a
parse parser res = either throwString pure $ do
  value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
  Aeson.Types.parseEither parser value
