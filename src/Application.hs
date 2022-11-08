{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Application where

import ClassyPrelude.Yesod (ReaderT, fromString, newManager, pack, runMigration, unpack, (</>))
import Data.Yaml.Aeson (decodeFileEither)
import GenericOIDC (oidcAuth')
import System.Directory (createDirectoryIfMissing)
import Text.Cassius
import Text.Julius
import URI.ByteString ()
import InterdimensionalConfig
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2.Prelude
import Yesod.Form.Bootstrap3
import Yesod.Static
import Prelude

data Interdimensional = Interdimensional
  { httpManager :: Manager,
    config :: InterdimensionalConfig,
    getStatic :: Static
  }

mkYesod
  "Interdimensional"
  [parseRoutes|
/auth                  AuthR Auth getAuth
/                      HomeR GET
|]

css :: p -> Css
css =
  [cassius|
body
  margin: 40px auto
  max-width: 650px
  line-height: 1.6
  font-size: 18px
  color: #444
  padding: 0 10px
h1, h2, h3
  line-height: 1.2
|]

footer :: WidgetFor Interdimensional ()
footer = [whamlet|
<p>
  <a href=@{HomeR}>Home</a>
|]

instance Yesod Interdimensional where
  approot = ApprootMaster $ host . config
  authRoute _ = Just $ AuthR LoginR

  isAuthorized _ _ = return Authorized

isSignedIn :: HandlerFor Interdimensional AuthResult
isSignedIn = do
  user <- maybeAuthId
  return $ case user of
    Nothing -> AuthenticationRequired
    Just _ -> Authorized

instance YesodAuth Interdimensional where
  type AuthId Interdimensional = Text
  authenticate = return . Authenticated . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins y = [oidcAuth' $ oidc $ config y]
  maybeAuthId = lookupSession "_ID"

instance RenderMessage Interdimensional FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
  user <- maybeAuthId
  mmsg <- getMessage
  defaultLayout
    [whamlet|
          ^{css}
          <h1>Welcome to Interdimensional!
          <p>Interdimensional is a free application portal, made for homelabs. It lets users see what applications
            they can access, via OpenID Connect.
          <p>It is written in 
            <a href="https://www.haskell.org/">Haskell
            and uses the 
            <a href="https://www.yesodweb.com/">Yesod
            web framework. You can find the source code
            <a href="https://github.com/dfsek/interdimensional">Here</a>.
            Enjoy!
          $maybe un <- user
            <p>Logged in as #{un}
            <p>
              <a href=@{AuthR LogoutR}>Log out
          $nothing
            <p>
              <a href=@{AuthR LoginR}>Log in
          $maybe msg <- mmsg
            <p>#{msg}
          ^{footer}
        |]


appMain :: IO ()
appMain = do
  c' <- decodeFileEither "config.yml"
  case c' of
    Left e -> error $ "Could not parse config file: " <> show e
    Right conf -> do
      let contentDir = static_dir conf
      createDirectoryIfMissing True contentDir
      putStrLn $ "Serving static content from " <> contentDir

      staticRoute <- static contentDir
      putStrLn $ "Launching application at " <> show (host conf)

      manager <- newManager

      warp 3001 $ Interdimensional manager conf staticRoute
