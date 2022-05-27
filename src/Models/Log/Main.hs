module Models.Log.Main
  ( Log(..)
  ) where

import RIO
import qualified System.IO
import qualified Codec.Serialise as Serialise
import qualified Models.Build.Main as Build
import qualified Models.StepName.Main as StepName
import qualified Models.LogCollectionStatus.Main as LogCollectionStatus

data Log = Log
  { output :: ByteString
  , step   :: StepName.StepName
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)
