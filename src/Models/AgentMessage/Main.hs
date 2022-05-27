module Models.AgentMessage.Main
  ( AgentMessage(..)
  ) where

import RIO
import qualified Codec.Serialise as Serialise
import qualified Models.Build.Main as Build
import qualified Models.BuildNumber.Main as BuildNumber
import qualified Models.Log.Main as Log

data AgentMessage =
    LogCollected BuildNumber.BuildNumber Log.Log
  | BuildUpdated BuildNumber.BuildNumber Build.Build
  deriving (Eq, Show, Generic, Serialise.Serialise)
