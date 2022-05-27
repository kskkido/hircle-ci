module Modules.Docker.Models.VolumeCreateOptions.Main
  ( VolumeCreateOptions(..)
  , empty
  , toJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson

data VolumeCreateOptions = VolumeCreateOptions
  { label :: Text
  }
  deriving (Eq, Show)

empty :: VolumeCreateOptions
empty = VolumeCreateOptions ""

toJSON :: VolumeCreateOptions -> Aeson.Value
toJSON options = Aeson.object
  [ ("Labels", Aeson.object [("hircle-ci", "")])
  ]

