module Services.Server.Modules.Job.Main
  ( Module(..)
  , fromIO
  ) where

import RIO
import qualified RIO.Map as Map
import qualified System.IO
import qualified Control.Concurrent.STM as STM
import qualified Modules.Github.Models.Commit.Main as Commit
import qualified Models.Job.Main as Job
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.StepName.Main as StepName
import qualified Models.BuildNumber.Main as BuildNumber
import qualified Models.AgentCommand.Main as AgentCommand
import qualified Models.AgentMessage.Main as AgentMessage
import qualified Models.JobStore.Main as JobStore

data Module = Module
  { queueJob        :: Pipeline.Pipeline -> Commit.Commit -> System.IO.IO BuildNumber.BuildNumber
  , findJob         :: BuildNumber.BuildNumber -> System.IO.IO (Maybe Job.Job)
  , getLog          :: BuildNumber.BuildNumber -> StepName.StepName -> System.IO.IO (Maybe ByteString)
  , getJobs         :: System.IO.IO [(BuildNumber.BuildNumber, Job.Job)]
  , dispatchCommand :: System.IO.IO (Maybe AgentCommand.AgentCommand)
  , processMessage  :: AgentMessage.AgentMessage -> System.IO.IO ()
  }

fromIO :: System.IO.IO Module
fromIO = do
  state <- STM.newTVarIO $ JobStore.JobStore
    { JobStore.jobs = Map.empty
    , JobStore.logs = Map.empty
    }
  return $ Module
    { queueJob = \px cx -> STM.atomically do
        STM.stateTVar state $ JobStore.queueJob px cx
    , findJob  = \bn -> STM.atomically do
        js <- STM.readTVar state
        return $ JobStore.toJob bn js
    , getLog   = \bn sn -> STM.atomically do
        js <- STM.readTVar state
        return $ JobStore.toLog bn sn js
    , getJobs  = STM.atomically do
        js <- STM.readTVar state
        return $ JobStore.toLatestJobs js
    , dispatchCommand = STM.atomically do
        STM.stateTVar state $ JobStore.dequeueJob
    , processMessage = \msx -> STM.atomically do
        STM.modifyTVar' state $ JobStore.processMessage msx
    }
