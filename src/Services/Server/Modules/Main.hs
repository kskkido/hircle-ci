module Services.Server.Modules.Main
  ( Module(..)
  , fromIO
  ) where

import RIO
import qualified System.IO
import qualified Services.Server.Modules.Job.Main as Job

data Module = Module
  { job :: Job.Module
  }

fromIO :: System.IO.IO Module
fromIO = do
  job <- Job.fromIO
  return $ Module
    { job = job
    }
