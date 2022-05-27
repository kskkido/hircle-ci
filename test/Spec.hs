import Test.Hspec
import RIO
import qualified RIO.Map
import qualified RIO.Set
import qualified RIO.HashMap as HashMap
import qualified Data.Aeson as Aeson
import qualified RIO.ByteString
import qualified System.IO
import qualified System.Process.Typed
import qualified Data.Yaml
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import qualified Control.Concurrent.Async as Async
import qualified Services.Build.Main as Services.Build
import qualified Services.Build.Modules.Main as Services.Build.Modules
import qualified Services.Agent.Main as Services.Agent
import qualified Services.Agent.Modules.Main as Services.Agent.Modules
import qualified Services.Server.Main as Services.Server
import qualified Services.Server.Modules.Main as Services.Server.Modules
import qualified Modules.Docker.Main as Modules.Docker
import qualified Modules.Docker.Models.ContainerExitCode.Main as Modules.Docker.Models.ContainerExitCode
import qualified Modules.Docker.Models.VolumeCreateOptions.Main as VolumeCreateOptions
import qualified Models.BuildHooks.Main as Models.BuildHooks
import qualified Models.BuildResult.Main as Models.BuildResult
import qualified Models.BuildState.Main as Models.BuildState
import qualified Models.BuildNumber.Main as Models.BuildNumber
import qualified Models.StepResult.Main as Models.StepResult
import qualified Models.AgentConfig.Main as Models.AgentConfig
import qualified Models.ServerConfig.Main as Models.ServerConfig
import qualified Models.JobState.Main as Models.JobState
import qualified Models.JobStore.Main as Models.JobStore
import qualified Models.JobQueuedResponse.Main as Models.JobQueuedResponse
import qualified Fixtures.Models.Build.Main as Fixtures.Models.Build
import qualified Fixtures.Models.Pipeline.Main as Fixtures.Models.Pipeline
import qualified Fixtures.Modules.Github.Models.Commit.Main as Fixtures.Modules.Github.Models.Commit

main :: System.IO.IO ()
main = do
  hspec do
    beforeAll Modules.Docker.clean $ describe "haskell-ci" do
      it "should run a success build" do
        build <- Services.Build.run Fixtures.Models.Build.test1 Models.BuildHooks.unit Services.Build.Modules.simple
        build.state `shouldBe` Models.BuildState.Complete Models.BuildResult.Success
        RIO.Map.elems build.completedSteps `shouldBe` [Models.StepResult.Success, Models.StepResult.Success]
      it "should run a failure build" do
        build <- Services.Build.run Fixtures.Models.Build.test2 Models.BuildHooks.unit Services.Build.Modules.simple
        build.state `shouldBe` Models.BuildState.Complete Models.BuildResult.Failure
        RIO.Map.elems build.completedSteps `shouldBe` [Models.StepResult.Success, Models.StepResult.Failure $ Modules.Docker.Models.ContainerExitCode.ContainerExitCode 1]
      it "should should persist workspaces between steps" do
        let env = Services.Build.Modules.simple
        volume  <- env.docker.createVolume $ VolumeCreateOptions.empty
        fixture <- return $ Fixtures.Models.Build.test3 volume.name
        build   <- Services.Build.run fixture Models.BuildHooks.unit env
        build.state `shouldBe` Models.BuildState.Complete Models.BuildResult.Success
        RIO.Map.elems build.completedSteps `shouldBe` [Models.StepResult.Success, Models.StepResult.Success]
      it "should collect logs" do
        cache <- newMVar $ RIO.Set.fromList ["hello", "world", "Linux"]
        let hooks = Models.BuildHooks.BuildHooks
                      { Models.BuildHooks.onLogCollected = \lx -> do
                          lxs <- readMVar cache
                          forM_ lxs $ \word -> do
                            case RIO.ByteString.breakSubstring word lx.output of
                              (_, "") -> pure ()
                              _       -> modifyMVar_ cache (pure . RIO.Set.delete word)
                      , Models.BuildHooks.onBuildUpdated = return . const ()
                      }
        build <- Services.Build.run Fixtures.Models.Build.test4 hooks Services.Build.Modules.simple
        build.state `shouldBe` Models.BuildState.Complete Models.BuildResult.Success
        RIO.Map.elems build.completedSteps `shouldBe` [Models.StepResult.Success, Models.StepResult.Success]
        readMVar cache >>= (`shouldBe` RIO.Set.empty)
      it "should pull images" do
        let env = Services.Build.Modules.simple
        System.Process.Typed.readProcessStdout "docker rmi -f busybox"
        build <- Services.Build.run Fixtures.Models.Build.test5 Models.BuildHooks.unit env
        build.state `shouldBe` Models.BuildState.Complete Models.BuildResult.Success
        RIO.Map.elems build.completedSteps `shouldBe` [Models.StepResult.Success, Models.StepResult.Success]
      it "should decode pipelines" do
        let env = Services.Build.Modules.simple
        pipeline <- Data.Yaml.decodeFileThrow "test/Fixtures/pipeline.yml"
        build    <- Services.Build.run (Fixtures.Models.Build.test6 pipeline) Models.BuildHooks.unit env
        build.state `shouldBe` Models.BuildState.Complete Models.BuildResult.Failure
      it "should run server and agent" do
        port      <- return $ 9000
        endpoint  <- return $ "http://localhost:" <> show port
        agentEnv  <- return $ Services.Agent.Modules.simple
        serverEnv <- Services.Server.Modules.fromIO
        tx <- Async.async do
          Services.Server.run (Models.ServerConfig.ServerConfig port) serverEnv
        ty <- Async.async do
          Services.Agent.run (Models.AgentConfig.AgentConfig endpoint) agentEnv
        Async.link tx
        Async.link ty
        bnx <- serverEnv.job.queueJob Fixtures.Models.Pipeline.test1 Fixtures.Modules.Github.Models.Commit.test1
        let iter = do
              mjx <- serverEnv.job.findJob bnx
              maybe iter return $ do
                jx <- mjx
                bx <- Models.JobState.toBuild jx.state
                Models.BuildState.toBuildResult bx.state
        brx <- iter
        brx `shouldBe` Models.BuildResult.Success
        Async.cancel tx
        Async.cancel ty
      it "should process webhooks" do
        port      <- return $ 9000
        endpoint  <- return $ "http://localhost:" <> show port
        agentEnv  <- return $ Services.Agent.Modules.simple
        serverEnv <- Services.Server.Modules.fromIO
        tx <- Async.async do
          Services.Server.run (Models.ServerConfig.ServerConfig port) serverEnv
        ty <- Async.async do
          Services.Agent.run (Models.AgentConfig.AgentConfig endpoint) agentEnv
        Async.link tx
        Async.link ty
        req <- HTTP.parseRequest endpoint <&> \req ->
           req
           & HTTP.setRequestMethod "POST"
           & HTTP.setRequestPath "/webhook/github"
           & HTTP.setRequestBodyFile "test/Fixtures/github-webhook-payload.json"
        res <- HTTP.getResponseBody <$> HTTP.httpBS req
        rsx <- either throwString return $ do
          json <- Aeson.eitherDecodeStrict res
          Aeson.Types.parseEither Models.JobQueuedResponse.fromJSON json
        rsx.status `shouldBe` "job queued"
        let iter = do
              mjx <- serverEnv.job.findJob rsx.number
              maybe iter return $ do
                jx <- mjx
                bx <- Models.JobState.toBuild jx.state
                Models.BuildState.toBuildResult bx.state
        brx <- iter
        brx `shouldBe` Models.BuildResult.Success
        Async.cancel tx
        Async.cancel ty
