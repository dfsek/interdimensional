{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Match (Match, fromQuery, MatchConfig (..), match) where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Maybe
import Data.Text (Text, splitOn)
import Data.Yaml ()
import GHC.Generics (Generic)

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
  match jsonData (Exists e) = isJust $ fromQuery e id jsonData
  match jsonData (InList e v) = elem v $ fromQueryTraverse e _String jsonData


assembleLens :: (AsValue t, Applicative f) => ((t -> f t) -> c) -> Text -> (Value -> f Value) -> c
assembleLens f token = f . key token

tokens :: Text -> [Text]
tokens = splitOn "."

foldQuery :: Applicative f => Text -> (a -> Value -> f Value) -> a -> Value -> f Value
foldQuery query get = foldl assembleLens id (tokens query) . get

fromQuery :: Text -> Prism' Value a -> Value -> Maybe a
fromQuery string get = (^? foldQuery string get)

fromQueryTraverse :: Text -> Prism' Value a -> Value -> [a]
fromQueryTraverse string get = (^.. foldQuery string (values . get))
