module Util(lazyBStoStrictText) where

import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Text as T
import Data.Text.Encoding as T

lazyBStoStrictText :: BL.ByteString -> T.Text
lazyBStoStrictText = T.decodeUtf8 . B.concat . BL.toChunks
