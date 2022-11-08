{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module InterdimensionalConfig (OIDCConfig (..), InterdimensionalConfig (..)) where

import Apps (AppConfig)
import ClassyPrelude.Conduit (Generic)
import Data.Aeson
import Data.Text (Text)
import URI.ByteString
import URI.ByteString.Aeson ()

data InterdimensionalConfig = InterdimensionalConfig
  { oidc :: OIDCConfig,
    host :: Text,
    static_dir :: String,
    apps :: [AppConfig]
  }
  deriving (Generic, FromJSON)

data OIDCConfig = OIDCConfig
  { secret :: Text,
    client_id :: Text,
    token_url :: URIRef Absolute,
    auth_url :: URIRef Absolute,
    user_info :: URIRef Absolute,
    username_attribute :: Text,
    plugin_name :: Text
  }
  deriving (Generic, FromJSON)
