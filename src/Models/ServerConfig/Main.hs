module Models.ServerConfig.Main
  ( ServerConfig(..)
  ) where

import RIO

data ServerConfig = ServerConfig
  { port :: Int
  } deriving (Eq, Show)
