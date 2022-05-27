module Services.Log.Modules.Main
  ( Module(..)
  , simple
  ) where

import qualified Services.Log.Modules.Docker.Main as Docker

data Module = Module
  { docker :: Docker.Module
  }

simple :: Module
simple = Module
  { docker = Docker.simple
  }
