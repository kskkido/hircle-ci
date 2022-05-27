module Modules.Docker.Models.ImageCreateOptions.Main
  ( ImageCreateOptions(..)
  , fromImage
  , toUrlQuery
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Modules.Docker.Models.Image.Main as Image

data ImageCreateOptions = ImageCreateOptions
  { image :: Text
  , tag   :: Text
  }
  deriving (Eq, Show)

fromImage :: Image.Image -> ImageCreateOptions
fromImage ix = ImageCreateOptions ix.name ix.tag

toUrlQuery :: ImageCreateOptions -> Text
toUrlQuery options = Text.intercalate "&"
  [ "fromImage=" <> options.image
  , "tag="       <> options.tag
  ]
