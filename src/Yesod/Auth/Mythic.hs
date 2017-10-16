{-# LANGUAGE FlexibleContexts #-}
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
import Yesod.Form
import Yesod.Core

-- an auth plugin needs three things: a name, a dispatch function, and a login
-- widget
authMythic :: YesodAuth m => AuthPlugin m
authMythic = AuthPlugin "mythic" dispatch loginWidget

-- arguments to dispatch are the HTTP method and a list of path elements
dispatch :: YesodAuth master =>
              Text -> [Text] -> AuthHandler master TypedContent
dispatch "POST" ["login"] = postLoginR >>= sendResponse
dispatch _ _ = notFound

data MythicLoginForm = MythicLoginForm Text Text

-- ghc cannot infer this type ("Couldn't match type ... because type variable
-- master would escape its scope")
postLoginR :: YesodAuth m => HandlerT Auth (HandlerT m IO) TypedContent
postLoginR = do
  result <- lift $ runInputPostResult $ MythicLoginForm
                    <$> ireq textField "ident"
                    <*> ireq textField "password"
  case result of
    FormSuccess (MythicLoginForm ident _) -> lift $ setCredsRedirect $ Creds "mythic" ident []
    _ -> loginErrorMessageI LoginR Msg.InvalidLogin

-- the login widget is called with a single argument which is a function that
-- converts a route in the auth subsite to a route in the parent site
loginWidget :: YesodAuth m =>
  (Route Auth -> Route m) -> WidgetT m IO ()
loginWidget convertRoute = do
  (widget, enctype) <- liftWidgetT $ generateFormPost loginForm
  [whamlet|
$newline never
<form method="post" action="@{convertRoute url}" enctype=#{enctype}>
    ^{widget}
    <input type="submit" value="Mythic Login">
|]
  where
    loginForm csrf = do
      (identRes, identView) <- mreq textField identSettings Nothing
      (passwordRes, passwordView) <- mreq passwordField passwordSettings Nothing
      let mythicRes = MythicLoginForm <$> identRes <*> passwordRes
          widget = do
            [whamlet|
              #{csrf}
              <div>
                ^{fvInput identView}
              <div>
                ^{fvInput passwordView}
            |]
      return (mythicRes, widget)
    url = PluginR "mythic" ["login"]
    identSettings = FieldSettings
                      { fsLabel = "Email or account number"
                      , fsTooltip = Nothing
                      , fsId = Just "ident"
                      , fsName = Just "ident"
                      , fsAttrs = [("autofocus", ""), ("placeholder", "email")]
                      }
    passwordSettings = FieldSettings
                      { fsLabel = "Password"
                      , fsTooltip = Nothing
                      , fsId = Just "password"
                      , fsName = Just "password"
                      , fsAttrs = [("placeholder", "password")]
                      }
