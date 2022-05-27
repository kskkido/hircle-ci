module Models.AgentConfig.Main
  ( AgentConfig(..)
  ) where

import RIO

data AgentConfig = AgentConfig
  { endpoint :: String
  } deriving (Eq, Show)
