module Services.Build.Modules.Main
  ( Module(..)
  , simple
  ) where

import qualified Services.Build.Modules.Docker.Main as Docker
import qualified Services.Build.Modules.Log.Main as Log

data Module = Module
  { docker :: Docker.Module
  , log    :: Log.Module
  }

simple :: Module
simple = Module
  { docker = Docker.simple
  , log    = Log.simple
  }
