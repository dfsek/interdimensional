{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Match (Match, fromQuery, MatchConfig (..), match) where

import Control.Lens (Prism', (^?), (%~), (&))
import Data.Aeson.Lens
import Data.Aeson
import Data.Maybe
import Data.Text (Text, splitOn)
import Data.Yaml (FromJSON, Value)
import GHC.Generics (Generic)
import Lens.Micro ((^..))
import Data.Vector (toList)
import Debug.Trace (trace, traceShowId)

class Match a where
  match :: Value -> a -> Bool

data MatchConfig
  = Exists
      { key_search :: Text
      }
  | InList
      { key_search :: Text,
        value :: Text
      }
  deriving (Eq, Generic, Show, FromJSON)

instance Match MatchConfig where
  match json (Exists e) = isJust $ fromQuery e id json
  match json (InList e v) = elem v $ traceShowId list
    where
      val :: Maybe Value
      val = traceShowId $ fromQuery e id json
      list = case val of
        Nothing -> []
        Just (Array v) -> catMaybes $ (^? _String) <$> toList v

fromQuery :: Text -> Prism' Value a -> Value -> Maybe a
fromQuery string get = (^? foldl append id tokens . get)
  where
    tokens = splitOn "." string
    append f token = f . key token
