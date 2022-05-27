module Modules.Docker.Models.ContainerExitCode.Main
  ( ContainerExitCode(..)
  , toInt
  ) where

import RIO
import qualified Codec.Serialise as Serialise

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show, Generic, Serialise.Serialise)

toInt :: ContainerExitCode -> Int
toInt (ContainerExitCode n) = n

