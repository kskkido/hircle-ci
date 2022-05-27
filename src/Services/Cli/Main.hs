module Services.Cli.Main
  ( main
  , runCommand
  ) where

import RIO
import qualified System.IO
import qualified System.Log.Logger as Logger
import qualified UI.Butcher.Monadic as Butcher
import qualified Models.CliCommand.Main as CliCommand
import qualified Models.AgentConfig.Main as AgentConfig
import qualified Models.ServerConfig.Main as ServerConfig
import qualified Services.Agent.Main as Services.Agent
import qualified Services.Agent.Modules.Main as Services.Agent.Modules
import qualified Services.Server.Main as Services.Server
import qualified Services.Server.Modules.Main as Services.Server.Modules

main :: System.IO.IO ()
main = Butcher.mainFromCmdParserWithHelpDesc $ \hd -> do
  Butcher.addCmdSynopsis "Hircle-CI command line utility"
  Butcher.addHelpCommand2 hd
  Butcher.addCmd "start-agent" do
    Butcher.addCmdSynopsis "Start agent node"
    ex <- Butcher.addParamString "ENDPOINT" $
      Butcher.paramHelpStr "Server endpoint"
      <> Butcher.paramSuggestions ["http://localhost:9000"]
      <> Butcher.paramDefault "http://localhost:9000"
    Butcher.addCmdImpl do
      runCommand $ CliCommand.StartAgent AgentConfig.AgentConfig
        { AgentConfig.endpoint = ex
        }
  Butcher.addCmd "start-server" do
    Butcher.addCmdSynopsis "Start server node"
    px <- Butcher.addParamString "PORT" $
      Butcher.paramHelpStr "Server port"
      <> Butcher.paramDefault (show 9000)
    Butcher.addCmdImpl do
      case readMaybe px of
        Nothing ->
          throwString "Port must be a number"
        Just px -> do
          runCommand $ CliCommand.StartServer ServerConfig.ServerConfig
            { ServerConfig.port = px
            }

runCommand :: CliCommand.CliCommand -> System.IO.IO ()
runCommand cmx =
  case cmx of
    CliCommand.StartAgent  cx -> do
      Logger.infoM "hircle-ci.agent" "Starting"
      Services.Agent.run cx Services.Agent.Modules.simple
    CliCommand.StartServer cx -> do
      Logger.infoM "hircle-ci.server" "Starting"
      env <- Services.Server.Modules.fromIO
      Services.Server.run cx env

