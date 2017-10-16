{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

-- | Provides a dummy authentication module that simply lets a user specify
-- his/her identifier. This is not intended for real world use, just for
-- testing.
module Yesod.Auth.Mythic
    ( authMythic
    ) where

import Data.Text (Text)
import Yesod.Auth
-- Y.A.Message defines internationalized messages to be used with
-- loginErrorMessageI
import qualified Yesod.Auth.Message as Msg
import Yesod.Form (FormResult(..), runInputPostResult, textField, ireq)
import Yesod.Core

authMythic :: YesodAuth m => AuthPlugin m
authMythic = AuthPlugin "mythic" dispatch login

-- arguments to dispatch are the HTTP method and a list of path elements
dispatch :: YesodAuth master =>
              Text -> [Text] -> AuthHandler master TypedContent
dispatch "POST" ["login"] = postLoginR >>= sendResponse
dispatch _ _ = notFound

-- ghc cannot infer this type ("Couldn't match type ... because type variable
-- master would escape its scope")
postLoginR :: YesodAuth m => HandlerT Auth (HandlerT m IO) TypedContent
postLoginR = do
  result <- lift $ runInputPostResult $ ireq textField "ident"
  case result of
    FormSuccess ident -> lift $ setCredsRedirect $ Creds "mythic" ident []
    _ -> loginErrorMessageI LoginR Msg.InvalidLogin

login authToMaster = do
    request <- getRequest
    toWidget [hamlet|
$newline never
<form method="post" action="@{authToMaster url}">
    $maybe t <- reqToken request
        <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
    Your new identifier is: #
    <input type="text" name="ident">
    <input type="submit" value="Mythic Login">
|]
  where
    url = PluginR "mythic" ["login"]
