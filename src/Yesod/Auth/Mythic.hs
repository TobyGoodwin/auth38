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
import qualified Data.Text as T
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

-- arguments to dispatch are the HTTP method and a list of path elements. the
-- "GET /login" case does not need to be handled explicitly: the framework will
-- automatically display the login widget in this case
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
    FormSuccess (MythicLoginForm ident pass)
      | ident == T.reverse pass
        -> lift $ setCredsRedirect $ Creds "mythic" ident []
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
      -- the next line is standard yesod form stuff, but if <$> and <*> here
      -- bother you, you may want to revise applicative functors
      -- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors
      -- the functor in question here is FormResult, look up its Applicative
      -- instance to see exactly what's happening
      let mythicRes = MythicLoginForm <$> identRes <*> passwordRes
      let widget = do
            [whamlet|
              #{csrf}
              <div>
                Username:
                ^{fvInput identView}
              <div>
                Password:
                ^{fvInput passwordView}
            |]
      return (mythicRes, widget)
    url = PluginR "mythic" ["login"]
    identSettings = FieldSettings
                      { fsLabel = "unused"
                      , fsTooltip = Nothing
                      , fsId = Just "ident"
                      , fsName = Just "ident"
                      , fsAttrs = [("autofocus", ""), ("placeholder", "username")]
                      }
    passwordSettings = FieldSettings
                      { fsLabel = "unused"
                      , fsTooltip = Nothing
                      , fsId = Just "password"
                      , fsName = Just "password"
                      , fsAttrs = [("placeholder", "password")]
                      }
