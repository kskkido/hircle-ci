module Modules.Docker.Models.Image.Main
  ( Image(..)
  , toText
  ) where

import RIO
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text.Partial
import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson

data Image = Image
  { name :: Text
  , tag  :: Text
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "parse-image" $ \image -> do
    case Text.Partial.splitOn ":" image of
      [name]      ->
        return $ Image
          { name = name
          , tag = "latest"
          }
      [name, tag] ->
        return $ Image
          { name = name
          , tag = tag
          }
      _           ->
        fail $ "Image has too many colons " <> Text.unpack image

toText :: Image -> Text
toText ix = Text.intercalate ":" [ix.name, ix.tag]
