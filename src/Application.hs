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

import Apps
import ClassyPrelude.Yesod (ReaderT, fromString, newManager, pack, runMigration, unpack, (</>))
import Data.Aeson (decodeStrict, eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.Yaml.Aeson (decodeFileEither)
import GenericOIDC
import InterdimensionalConfig
import System.Directory (createDirectoryIfMissing)
import Text.Cassius
import Text.Julius
import URI.ByteString ()
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2 (getUserResponseJSON)
import Yesod.Auth.OAuth2.Prelude
import Yesod.Form.Bootstrap3
import Yesod.Static
import Prelude
import Match
import Util

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
#apps_table
  display: table
  width: 100%
  border-collapse: collapse

.apps_row
  height: 50px

td
  padding: 10px
 
.apps_row:nth-child(even)
  background-color: rgb(43, 47, 49)

.app_icon
  height: 100%
  max-width: 175px;
  max-height: 50px;
  vertical-align: middle

a
  color: #3391ff

html
  color: rgb(189, 183, 175)
  background-color: #181a1b
body
  margin: 40px auto
  line-height: 1.6
  font-size: 18px
  padding: 0 10px
h1, h2, h3
  line-height: 1.2
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
  authenticate creds = do
    let json :: Either String Value
        json = getUserResponseJSON creds
        userName = credsIdent creds
    case json of
      Left e -> return $ ServerError $ pack $ "Unable to parse JSON" <> e
      Right j -> do
        liftIO $ do
          print "auth json"
          print $ toJsonText j
        setSession "_OIDC" $ toJsonText j
        return $ Authenticated userName
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
  interdimensional <- getYesod
  json <- (>>= (decodeStrict :: ByteString -> Maybe Value)) <$> lookupSessionBS "_OIDC"
  defaultLayout $ case json of
    Just j ->
      let idConfig = config interdimensional
          hasAccess app = case access app of
              PublicApp -> True
              AuthenticatedApp m -> match j m
          filtered = filter hasAccess $ apps idConfig
         
      in
      [whamlet|
              ^{css}
              <h1>Welcome to Interdimensional!
              $maybe un <- user
                <p>Logged in as #{un}
                <p>
                  <a href=@{AuthR LogoutR}>Log out
              $nothing
                <p>
                  <a href=@{AuthR LoginR}>Log in
              $maybe msg <- mmsg
                <p>#{msg}
              <h2>
                Your Apps
              <table #apps_table>
                <tr>
                  <td>
                    <b>App/Link
                  <td>
                    <b>Description
                  <td>
                    <b>Source Code
                $forall app <- filtered
                  <tr .apps_row>
                    <td>
                      <a href=#{uriToText (app_uri app)}>
                        $maybe img <- uriToText <$> image_path app
                          <img .app_icon src=#{img}> 
                        $nothing
                          #{name app}
                        
                    <td>
                      #{description app}
                    <td>
                      <a href=#{uriToText (source_uri app)}>
                        Source Code
            |]
    Nothing ->
      [whamlet|
        Error|] -- TODO - make not bad

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
