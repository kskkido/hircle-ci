module Modules.Github.Models.Commit.Main
  ( Commit(..)
  , fromWebhookBody
  , fromWebhookJSON
  ) where

import RIO
import qualified RIO.Text as Text
import qualified System.IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data Commit = Commit
  { repository :: Text
  , sha        :: Text
  , branch     :: Text
  , author     :: Text
  , message    :: Text
  } deriving (Eq, Show, Generic, Aeson.ToJSON)

fromWebhookBody :: LByteString -> Either String Commit
fromWebhookBody bs = do
  json <- Aeson.eitherDecode bs
  Aeson.Types.parseEither fromWebhookJSON json

fromWebhookJSON :: Aeson.Value -> Aeson.Types.Parser Commit
fromWebhookJSON = Aeson.withObject "webhook-payload" $ \o -> do
  repository <- o Aeson..: "repository" >>= \x -> x Aeson..: "full_name"
  branch     <- o Aeson..: "ref" <&> Text.dropPrefix "refs/heads/"
  commit     <- o Aeson..: "head_commit"
  sha        <- commit Aeson..: "id"
  author     <- commit Aeson..: "author" >>= \x -> x Aeson..: "username"
  message    <- commit Aeson..: "message"
  return $ Commit
    { repository = repository
    , sha        = sha
    , branch     = branch
    , author     = author
    , message    = message
    }

