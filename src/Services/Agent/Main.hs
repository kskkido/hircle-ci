module Services.Agent.Main
  ( run
  , sendMessage
  ) where

import RIO
import qualified System.IO
import qualified System.Log.Logger as Logger
import qualified Codec.Serialise as Serialise
import qualified Network.HTTP.Simple as HTTP
import qualified Models.AgentConfig.Main as AgentConfig
import qualified Models.AgentCommand.Main as AgentCommand
import qualified Models.AgentMessage.Main as AgentMessage
import qualified Models.Build.Main as Build
import qualified Models.BuildHooks.Main as BuildHooks
import qualified Models.BuildNumber.Main as BuildNumber
import qualified Services.Agent.Modules.Main as Modules

run :: AgentConfig.AgentConfig -> Modules.Module -> System.IO.IO ()
run config env = forever do
  endpoint <- HTTP.parseRequest config.endpoint
  let req = endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/pull"
  do
    body     <- HTTP.getResponseBody <$> HTTP.httpLBS req
    mcommand <- return $ (Serialise.deserialise body :: Maybe AgentCommand.AgentCommand)
    forM_ mcommand $ \command ->
      case command of
        AgentCommand.StartBuild bnx px -> do
          hooks <- return BuildHooks.BuildHooks
            { BuildHooks.onLogCollected = \lx -> sendMessage (AgentMessage.LogCollected bnx lx) config env
            , BuildHooks.onBuildUpdated = \bx -> sendMessage (AgentMessage.BuildUpdated bnx bx) config env
            }
          Logger.infoM "hircle-ci.agent" $ "Starting build " <> BuildNumber.toLogString bnx
          void $ (env.build.prepare px >>= (($ hooks) . env.build.run))
          Logger.infoM "hircle-ci.agent" $ "Finishing build " <> BuildNumber.toLogString bnx
    `catch` \e -> do
      Logger.warningM "hircle-ci.agent" "Server offline, waiting..."
      Logger.warningM "hircle-ci.agent" $ show (e :: HTTP.HttpException)
  threadDelay (1 * 1000 * 1000)

sendMessage :: AgentMessage.AgentMessage -> AgentConfig.AgentConfig -> Modules.Module -> System.IO.IO ()
sendMessage msg config env = do
  endpoint <- HTTP.parseRequest config.endpoint
  let req = endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/send"
          & HTTP.setRequestBodyLBS (Serialise.serialise msg)
  void $ HTTP.httpBS req
