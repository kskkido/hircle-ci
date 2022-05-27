module Models.BuildHooks.Main
  ( BuildHooks(..)
  , unit
  ) where

import RIO
import qualified System.IO
import qualified Models.Log.Main as Log
import qualified Models.Build.Main as Build

data BuildHooks = BuildHooks
  { onLogCollected :: Log.Log -> System.IO.IO ()
  , onBuildUpdated :: Build.Build -> System.IO.IO ()
  }

unit :: BuildHooks
unit = BuildHooks
  { onLogCollected = const $ return ()
  , onBuildUpdated = const $ return ()
  }

