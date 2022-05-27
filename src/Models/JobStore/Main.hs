module Models.JobStore.Main
  ( JobStore(..)
  , toJob
  , toLog
  , toLatestJobs
  , toNextBuildNumber
  , queueJob
  , dequeueJob
  , processMessage
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified System.IO
import qualified Control.Monad
import qualified Modules.Github.Models.Commit.Main as Commit
import qualified Models.AgentCommand.Main as AgentCommand
import qualified Models.AgentMessage.Main as AgentMessage
import qualified Models.BuildNumber.Main as BuildNumber
import qualified Models.BuildResult.Main as BuildResult
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.StepName.Main as StepName
import qualified Models.Log.Main as Log
import qualified Models.Job.Main as Job
import qualified Models.JobState.Main as JobState

data JobStore = JobStore
  { jobs :: Map BuildNumber.BuildNumber Job.Job
  , logs :: Map (BuildNumber.BuildNumber, StepName.StepName) ByteString
  }

toJob :: BuildNumber.BuildNumber -> JobStore -> Maybe Job.Job
toJob bn js = Map.lookup bn js.jobs

toLog :: BuildNumber.BuildNumber -> StepName.StepName -> JobStore -> Maybe ByteString
toLog bn sn js = Map.lookup (bn, sn) js.logs

toLatestJobs :: JobStore -> [(BuildNumber.BuildNumber, Job.Job)]
toLatestJobs js = List.reverse $ Map.toList js.jobs

toNextBuildNumber :: JobStore -> BuildNumber.BuildNumber
toNextBuildNumber js = BuildNumber.BuildNumber $ maybe 0 (+ 1) $ do
  bnx <- List.lastMaybe $ List.sort $ Map.keys js.jobs
  return $ BuildNumber.toInt bnx

queueJob :: Pipeline.Pipeline -> Commit.Commit -> JobStore -> (BuildNumber.BuildNumber, JobStore)
queueJob px cx js =
  let n = toNextBuildNumber js
      j = Job.Job
        { Job.pipeline = px
        , Job.commit   = cx
        , Job.state    = JobState.Queued
        }
      js' = js
        { jobs = Map.insert n j js.jobs
        }
  in (n, js')

dequeueJob :: JobStore -> (Maybe AgentCommand.AgentCommand, JobStore)
dequeueJob js = maybe (Nothing, js) id $ do
  let pxs = Map.toList js.jobs
  (bn, jx) <- flip List.find pxs $ \(_, jx) -> jx.state == JobState.Queued
  let cmx = AgentCommand.StartBuild bn jx.pipeline
      jsx = Map.insert bn (jx { Job.state = JobState.Assigned }) js.jobs
  return $ (pure cmx, js { jobs = jsx })

processMessage :: AgentMessage.AgentMessage -> JobStore -> JobStore
processMessage msx js =
  case msx of
    AgentMessage.BuildUpdated bn bx ->
      js { jobs = Map.adjust (Job.scheduleBuild bx) bn js.jobs }
    AgentMessage.LogCollected bn lx ->
      js { logs = Map.insertWith (flip mappend) (bn, lx.step) lx.output js.logs }
