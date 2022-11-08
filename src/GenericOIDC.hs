{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module GenericOIDC (oidcAuth, oidcAuth') where

import ClassyPrelude.Yesod (WidgetFor, whamlet)
import Data.ByteString.Lazy (ByteString)
import Network.OAuth.OAuth2.Compat (authGetBS)
import InterdimensionalConfig
import qualified Yesod.Auth.OAuth2.Exception as YesodOAuth2Exception
import Yesod.Auth.OAuth2.Prelude
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Text (unpack, splitOn)
import Prelude
import Lens.Micro ((^?))
import Control.Lens (Prism')


oidcAuth' :: YesodAuth m => OIDCConfig -> AuthPlugin m
oidcAuth' config = oidcAuth [whamlet|Login with #{plugin_name config}|] config

oidcAuth :: YesodAuth m => WidgetFor m () -> OIDCConfig -> AuthPlugin m
oidcAuth widget config =
  let pluginName = plugin_name config in
  authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
    resp <- authGetBS manager (accessToken token) (user_info config)
    userResponse <- fromAuthGet pluginName resp
    json <- decodeAuthJSON pluginName userResponse :: IO Value

    print json

    print $ fromQuery "resource_access" id json
    print $ fromQuery "preferred_username" _String json
    print $ fromQuery "resource_access.interdimensional" id json
    print $ fromQuery "resource_access.interdimensional.roles" id json

    let unKey = username_attribute config

    username <- case fromQuery "preferred_username" _String json of
      Nothing -> throwIO $ YesodOAuth2Exception.JSONDecodingError pluginName ("No such key " <> show unKey <> " in response.")
      Just s -> return s

    putStrLn $ "Username: " <> unpack username

    pure
      Creds
        { credsPlugin = pluginName,
          credsIdent = username,
          credsExtra = setExtra token userResponse
        }
  where
    oauth2 =
      OAuth2
        { oauth2ClientId = client_id config,
          oauth2ClientSecret = Just $ secret config,
          oauth2AuthorizeEndpoint = auth_url config `withQuery` [ scopeParam " " ["openid", "roles", "profile", "phone"]],
          oauth2TokenEndpoint = token_url config,
          oauth2RedirectUri = Nothing
        }

fromQuery :: Text -> Prism' Value a -> Value -> Maybe a
fromQuery string get = (^? foldl append id tokens . get) where
  tokens = splitOn "." string
  append f token = f . key token

fromAuthGet :: Text -> Either ByteString ByteString -> IO ByteString
fromAuthGet _ (Right bs) = pure bs -- nice
fromAuthGet name (Left err) =
  throwIO $ YesodOAuth2Exception.OAuth2Error name err

decodeAuthJSON :: Text -> ByteString -> IO Value
decodeAuthJSON name resp =
  case eitherDecode resp of
    Left err -> throwIO $ YesodOAuth2Exception.JSONDecodingError name err
    Right json -> return json

