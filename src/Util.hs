module Util(lazyBStoStrictText, uriToText, strictBStoStrictText) where

import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Text as T
import Data.Text.Encoding as T
import URI.ByteString

lazyBStoStrictText :: BL.ByteString -> T.Text
lazyBStoStrictText = T.decodeUtf8 . B.concat . BL.toChunks

strictBStoStrictText :: B.ByteString -> T.Text
strictBStoStrictText = T.decodeUtf8

uriToText :: URIRef Absolute -> Text
uriToText uri = strictBStoStrictText $ serializeURIRef' uri