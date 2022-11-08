{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Match(Match, fromQuery, MatchConfig(..)) where

import Data.Yaml (Value, FromJSON)
import Data.Text (Text, splitOn)
import Control.Lens (Prism', (^?))
import Data.Aeson.Lens
import Data.Maybe
import Lens.Micro ((^..))
import GHC.Generics (Generic)


class Match a where
  match :: a -> Value -> Bool

data MatchConfig =
  Exists {
  key_search :: Text
  }
  | InList {
  key_search :: Text,
  value :: Text
  }
  deriving (Eq, Generic, Show, FromJSON)

instance Match MatchConfig where
  match (Exists e) json = isJust $ fromQuery e id json
  match (InList e v) json = elem v $ val ^.. toList where
    val = fromQuery e id json
    toList = traverse . _String


fromQuery :: Text -> Prism' Value a -> Value -> Maybe a
fromQuery string get = (^? foldl append id tokens . get) where
  tokens = splitOn "." string
  append f token = f . key token