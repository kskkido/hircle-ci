module Modules.Docker.Models.ContainerStatus.Main
  ( ContainerStatus(..)
  , fromStateObject
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Modules.Docker.Models.ContainerExitCode.Main as ContainerExitCode


data ContainerStatus =
    Running
  | Unknown Text
  | Complete ContainerExitCode.ContainerExitCode
    deriving (Eq, Show)

fromStateObject :: Aeson.Object -> Aeson.Types.Parser ContainerStatus
fromStateObject state = do
  status <- state Aeson..: "Status"
  case status of
    "running" -> do
      return $ Running
    "exited"  -> do
      code <- state Aeson..: "ExitCode"
      return $ Complete $ ContainerExitCode.ContainerExitCode code
    other     -> do
      return $ Unknown other
