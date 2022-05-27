module Models.BuildNumber.Main
  ( BuildNumber(..)
  , toInt
  , toLogString
  ) where

import RIO
import qualified Codec.Serialise as Serialise

newtype BuildNumber = BuildNumber Int
  deriving (Eq, Show, Ord, Generic, Serialise.Serialise)

toInt :: BuildNumber -> Int
toInt (BuildNumber x) = x

toLogString :: BuildNumber -> String
toLogString bnx = "#" <> show (toInt bnx)
