module Models.JobQueuedResponse.Main
  ( JobQueuedResponse(..)
  , fromBuildNumber
  , fromJSON
  , toJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Models.BuildNumber.Main as BuildNumber

data JobQueuedResponse = JobQueuedResponse
  { number :: BuildNumber.BuildNumber
  , status :: Text
  } deriving (Eq, Show)

fromBuildNumber :: BuildNumber.BuildNumber -> JobQueuedResponse
fromBuildNumber bn = JobQueuedResponse
  { number = bn
  , status = "job queued"
  }

fromJSON :: Aeson.Value -> Aeson.Types.Parser JobQueuedResponse
fromJSON = Aeson.withObject "job-queued-response" $ \o -> do
  number <- o Aeson..: "number" <&> BuildNumber.BuildNumber
  status <- o Aeson..: "status"
  return $ JobQueuedResponse
    { number = number
    , status = status
    }

toJSON :: JobQueuedResponse -> Aeson.Value
toJSON rx = Aeson.object $
  [ ("number", Aeson.toJSON $ BuildNumber.toInt rx.number)
  , ("status", Aeson.toJSON $ rx.status)
  ]
