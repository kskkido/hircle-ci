module Services.Agent.Modules.Build.Main
  ( Module(..)
  , simple
  , fromBuildEnv
  ) where

import RIO
import qualified System.IO
import qualified Models.Build.Main as Models.Build
import qualified Models.BuildHooks.Main as Models.BuildHooks
import qualified Models.Pipeline.Main as Models.Pipeline
import qualified Services.Build.Main as Services.Build
import qualified Services.Build.Modules.Main as Services.Build.Modules

data Module = Module
  { run     :: Models.Build.Build -> Models.BuildHooks.BuildHooks -> System.IO.IO Models.Build.Build
  , prepare :: Models.Pipeline.Pipeline -> System.IO.IO Models.Build.Build
  }

simple :: Module
simple = fromBuildEnv Services.Build.Modules.simple

fromBuildEnv :: Services.Build.Modules.Module -> Module
fromBuildEnv env = Module
  { run     = \bx -> ($ env) . Services.Build.run bx
  , prepare = ($ env) . Services.Build.prepare
  }
