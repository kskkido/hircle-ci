module Modules.Timestamp.Main
  ( toText
  ) where

import RIO
import qualified Data.Time.Clock.POSIX as Time

toText :: Time.POSIXTime -> Text
toText t = tshow (round t :: Int)
