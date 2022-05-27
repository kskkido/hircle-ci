module Models.CliCommand.Main
  ( CliCommand(..)
  ) where

import RIO
import qualified Models.AgentConfig.Main as AgentConfig
import qualified Models.ServerConfig.Main as ServerConfig

data CliCommand =
    StartAgent AgentConfig.AgentConfig
  | StartServer ServerConfig.ServerConfig
  deriving (Eq, Show)
