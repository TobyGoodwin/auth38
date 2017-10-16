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
import Yesod.Form (runInputPost, textField, ireq)
import Yesod.Core

authMythic :: YesodAuth m => AuthPlugin m
authMythic = AuthPlugin "mythic" dispatch login

-- first argument to dispatch is the HTTP method, second is a list of Pieces
dispatch :: YesodAuth master =>
              Text -> [Text] -> AuthHandler master TypedContent
dispatch "POST" ["login"] = do
    ident <- lift $ runInputPost $ ireq textField "ident"
    lift $ setCredsRedirect $ Creds "mythic" ident []
dispatch _ _ = notFound

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
