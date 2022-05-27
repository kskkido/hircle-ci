module Services.Build.Main
  ( run
  , prepare
  , progress
  , process
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified System.IO
import qualified Modules.Docker.Models.ContainerCheckOptions.Main as ContainerCheckOptions
import qualified Modules.Docker.Models.ContainerCreateOptions.Main as ContainerCreateOptions
import qualified Modules.Docker.Models.ContainerStartOptions.Main as ContainerStartOptions
import qualified Modules.Docker.Models.ImageCreateOptions.Main as ImageCreateOptions
import qualified Modules.Docker.Models.VolumeCreateOptions.Main as VolumeCreateOptions
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID
import qualified Modules.Docker.Models.ContainerStatus.Main as Docker.ContainerStatus
import qualified Modules.Docker.Models.ContainerExitCode.Main as Docker.ContainerExitCode
import qualified Models.Build.Main as Build
import qualified Models.BuildHooks.Main as BuildHooks
import qualified Models.BuildResult.Main as BuildResult
import qualified Models.BuildState.Main as BuildState
import qualified Models.BuildRunningState.Main as BuildRunningState
import qualified Models.LogCollection.Main as LogCollection
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.Step.Main as Step
import qualified Models.StepName.Main as StepName
import qualified Models.StepResult.Main as StepResult
import qualified Services.Build.Modules.Main as Module

run :: Build.Build -> BuildHooks.BuildHooks -> Module.Module -> System.IO.IO Build.Build
run bx hx env = do
  let iter :: Build.Build -> LogCollection.LogCollection -> System.IO.IO Build.Build
      iter bx cx = do
        (cy, ls) <- env.log.collect bx cx
        traverse_ hx.onLogCollected ls
        by <- progress bx env
        hx.onBuildUpdated by
        case by.state of
          BuildState.Complete _ -> do
            pure by
          _                     -> do
            threadDelay (1 * 1000 * 1000)
            iter by cy
  iter bx $ LogCollection.fromPipeline bx.pipeline

prepare :: Pipeline.Pipeline -> Module.Module -> System.IO.IO Build.Build
prepare px env = do
  vx <- env.docker.createVolume $ VolumeCreateOptions.empty
  return $ (Build.fromPipeline px) { Build.volume = return $ vx.name }

progress :: Build.Build -> Module.Module -> System.IO.IO Build.Build
progress bx env =
  case bx.state of
    BuildState.Ready ->
      case process bx of
        Left  rx ->
          return $ bx
            { Build.state = BuildState.Complete rx
            }
        Right sx -> do
          env.docker.createImage $ ImageCreateOptions.fromImage sx.image
          response <- env.docker.createContainer $ Step.toContainerCreateOptions (bx.volume) sx
          env.docker.startContainer $ ContainerStartOptions.ContainerStartOptions response.containerID
          return $ bx
            { Build.state = BuildState.Running $ BuildRunningState.fromStep sx response.containerID
            }
    BuildState.Running sx -> do
      response <- env.docker.checkContainer $ ContainerCheckOptions.ContainerCheckOptions sx.containerID
      case response.status of
        Docker.ContainerStatus.Running     ->
          return bx
        Docker.ContainerStatus.Unknown ox  ->
          return $ bx
            { Build.state = BuildState.Complete $ BuildResult.Unknown ox
            }
        Docker.ContainerStatus.Complete ex -> do
          let result = StepResult.fromContainerExitCode ex
          return $ bx
            { Build.state = BuildState.Ready
            , Build.completedSteps = Map.insert sx.step result bx.completedSteps
            }
    BuildState.Complete _ ->
      return bx

process :: Build.Build -> Either BuildResult.BuildResult Step.Step
process bx = do
  maybe (Left BuildResult.Failure) (const $ Right ()) $ do
    guard $ List.all ((==) StepResult.Success) bx.completedSteps
  maybe (Left BuildResult.Success) Right $ do
    List.find (not . flip Map.member bx.completedSteps . Step.name) bx.pipeline.steps
