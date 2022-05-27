module Modules.Docker.Models.ContainerID.Main
  ( ContainerID(..)
  , toText
  ) where

import RIO
import qualified Codec.Serialise as Serialise

newtype ContainerID = ContainerID Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

toText :: ContainerID -> Text
toText (ContainerID x) = x
