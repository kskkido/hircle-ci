module Models.Pipeline.Main
  ( Pipeline(..)
  , prepend
  ) where

import RIO
import qualified RIO.NonEmpty as NonEmpty
import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified Models.Step.Main as Step

data Pipeline = Pipeline
  { steps :: NonEmpty Step.Step
  } deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

prepend :: Step.Step -> Pipeline -> Pipeline
prepend sx px = px { steps = NonEmpty.cons sx px.steps }
