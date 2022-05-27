module Services.Build.Modules.Log.Main
  ( Module(..)
  , simple
  ) where

import RIO
import qualified System.IO
import qualified Models.Build.Main as Build
import qualified Models.Log.Main as Log
import qualified Models.LogCollection.Main as LogCollection
import qualified Services.Log.Modules.Main as Services.Log.Modules
import qualified Services.Log.Main as Services.Log

data Module = Module
  { collect :: Build.Build -> LogCollection.LogCollection -> System.IO.IO (LogCollection.LogCollection, [Log.Log])
  }

simple :: Module
simple = Module
  { collect = \bx cx -> Services.Log.run bx cx Services.Log.Modules.simple
  }
