module Models.LogCollection.Main
  ( LogCollection
  , fromPipeline
  ) where

import RIO
import qualified RIO.Map
import qualified RIO.NonEmpty
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.StepName.Main as StepName
import qualified Models.LogCollectionStatus.Main as LogCollectionStatus

type LogCollection = Map StepName.StepName LogCollectionStatus.LogCollectionStatus

fromPipeline :: Pipeline.Pipeline -> LogCollection
fromPipeline px = (RIO.Map.fromList . RIO.NonEmpty.toList) $ px.steps <&> \step -> (step.name, LogCollectionStatus.Ready)
