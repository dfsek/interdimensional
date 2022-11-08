{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Apps (AppConfig (..), AccessConfig (..)) where

import ClassyPrelude.Conduit (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import URI.ByteString (Absolute, URIRef)
import URI.ByteString.Aeson ()

data AccessConfig
  = PublicApp
      {}
  | AuthenticatedApp
      {role :: Text}
  deriving (Eq, Show, FromJSON, Generic)

data AppConfig = AppConfig
  { name :: Text,
    app_uri :: URIRef Absolute,
    image_path :: Maybe Text,
    source_uri :: URIRef Absolute,
    description :: Text,
    access :: AccessConfig
  }
  deriving (Eq, Show, FromJSON, Generic)
