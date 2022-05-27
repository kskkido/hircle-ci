module Models.StepName.Main
  ( StepName(..)
  , toText
  ) where

import RIO
import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson

newtype StepName = StepName Text
  deriving (Eq, Show, Ord, Generic, Aeson.FromJSON, Serialise.Serialise)

toText :: StepName -> Text
toText (StepName text) = text
