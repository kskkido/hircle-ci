module Services.Agent.Modules.Main
  ( Module(..)
  , simple
  , fromBuildEnv
  ) where

import RIO
import qualified Services.Agent.Modules.Build.Main as Build

data Module = Module
  { build :: Build.Module
  }

simple :: Module
simple = Module
  { build = Build.simple
  }

fromBuildEnv = Module . Build.fromBuildEnv
