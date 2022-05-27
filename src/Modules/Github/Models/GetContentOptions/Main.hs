module Modules.Github.Models.GetContentOptions.Main
  ( GetContentOptions(..)
  ) where

import RIO

data GetContentOptions = GetContentOptions
  { sha        :: Text
  , repository :: Text
  , path       :: Text
  , userAgent  :: ByteString
  } deriving (Eq, Show)

