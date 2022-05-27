module Modules.Docker.Models.Volume.Main
  ( Volume(..)
  , toText
  ) where

import RIO
import qualified Codec.Serialise as Serialise

newtype Volume = Volume Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

toText :: Volume -> Text
toText (Volume text) = text
