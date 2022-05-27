module Services.Server.Main
  ( run
  ) where

import RIO
import qualified System.IO
import qualified System.Log.Logger as Logger
import qualified Data.Yaml
import qualified Web.Scotty as Scotty
import qualified Codec.Serialise as Serialise
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Models.BuildNumber.Main as BuildNumber
import qualified Models.Job.Main as Job
import qualified Models.Step.Main as Step
import qualified Models.StepName.Main as StepName
import qualified Models.Pipeline.Main as Pipeline
import qualified Models.ServerConfig.Main as ServerConfig
import qualified Models.JobQueuedResponse.Main as JobQueuedResponse
import qualified Modules.Github.Main as Github
import qualified Modules.Github.Models.Commit.Main as Commit
import qualified Modules.Github.Models.GetContentOptions.Main as GetContentOptions
import qualified Services.Server.Modules.Main as Modules

run :: ServerConfig.ServerConfig -> Modules.Module -> System.IO.IO ()
run config env = Scotty.scotty config.port do
  Scotty.middleware Cors.simpleCors
  Scotty.get "/build" do
    jxs <- Scotty.liftAndCatchIO $ env.job.getJobs
    Scotty.json $ jxs <&> \(bn,jx) -> Job.toJSON bn jx
  Scotty.get "/build/:number" do
    bn  <- BuildNumber.BuildNumber <$> Scotty.param "number"
    mjx <- Scotty.liftAndCatchIO (env.job.findJob bn)
    jx  <- maybe (Scotty.raiseStatus HTTP.Types.status404 "Build not found") return mjx
    Scotty.json $ Job.toJSON bn jx
  Scotty.get "/build/:number/steps/:step/logs" do
    bn <- BuildNumber.BuildNumber <$> Scotty.param "number"
    sn <- StepName.StepName <$> Scotty.param "step"
    lx <- Scotty.liftAndCatchIO $ env.job.getLog bn sn
    Scotty.raw $ fromStrictBytes $ fromMaybe "" lx
  Scotty.post "/agent/pull" do
    command <- Scotty.liftAndCatchIO do
      env.job.dispatchCommand
    Scotty.raw $ Serialise.serialise command
  Scotty.post "/agent/send" do
    message <- Serialise.deserialise <$> Scotty.body
    Scotty.liftAndCatchIO do
      env.job.processMessage message
    Scotty.json ("message processed" :: Text)
  Scotty.post "/webhook/github" do
    body <- Scotty.body
    res  <- Scotty.liftAndCatchIO do
      commit   <- either throwString pure $ Commit.fromWebhookBody body
      content  <- Github.getContent GetContentOptions.GetContentOptions
        { GetContentOptions.sha = commit.sha
        , GetContentOptions.repository = commit.repository
        , GetContentOptions.path = ".quad.yml"
        , GetContentOptions.userAgent = "quad-ci"
        }
      pipeline <- Pipeline.prepend (Step.fromCommit commit) <$> Data.Yaml.decodeThrow content
      res <- JobQueuedResponse.fromBuildNumber <$> env.job.queueJob pipeline commit
      Logger.infoM "hircle-ci.server" $ "Queued job " <> BuildNumber.toLogString res.number
      return res
    Scotty.json $ JobQueuedResponse.toJSON res

