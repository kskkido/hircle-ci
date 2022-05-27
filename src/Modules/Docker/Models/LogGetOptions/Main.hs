module Modules.Docker.Models.LogGetOptions.Main
  ( LogGetOptions(..)
  , toUrlQuery
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Data.Time.Clock.POSIX as Time
import qualified Modules.Timestamp.Main as Timestamp
import qualified Modules.Docker.Models.ContainerID.Main as ContainerID

data LogGetOptions = LogGetOptions
  { containerID :: ContainerID.ContainerID
  , since :: Time.POSIXTime
  , until :: Time.POSIXTime
  }
    deriving (Eq, Show)

toUrlQuery :: LogGetOptions -> Text
toUrlQuery options = Text.intercalate "&"
  [ "stdout=true"
  , "stderr=true"
  , "since=" <> Timestamp.toText options.since
  , "until=" <> Timestamp.toText options.until
  ]
