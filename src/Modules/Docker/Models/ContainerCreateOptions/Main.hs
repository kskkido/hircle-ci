module Modules.Docker.Models.ContainerCreateOptions.Main
  ( ContainerCreateOptions(..)
  , toJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Modules.Docker.Models.Image.Main as Image
import qualified Modules.Docker.Models.Volume.Main as Volume

data ContainerCreateOptions = ContainerCreateOptions
  { image  :: Image.Image
  , script :: Text
  , volume :: Maybe Volume.Volume
  }
  deriving (Eq, Show)

toJSON :: ContainerCreateOptions -> Aeson.Value
toJSON options = Aeson.object $
  [ ("Image",      Aeson.toJSON $ Image.toText options.image)
  , ("Tty",        Aeson.toJSON True)
  , ("Labels",     Aeson.object [("hircle-ci", "")])
  , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
  , ("Env",        Aeson.toJSON ["HIRCLE_CI_SCRIPT=" <> options.script])
  , ("Cmd",        "echo \"$HIRCLE_CI_SCRIPT\" | /bin/sh")
  ] <> (
    maybe [] id $ do
      volume <- options.volume
      bind   <- return $ Volume.toText volume <> ":/app"
      return $
        [ ("WorkingDir", "/app")
        , ("HostConfig", Aeson.object [("Binds", Aeson.toJSON [bind])])
        ]
  )

