{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server(app) where

import Control.Monad.Reader

import Servant
import Servant.Auth.Server (JWTSettings, CookieSettings, defaultCookieSettings)

import App
import API.API (API, apiProxy)
import Handler.Auth (authHandler)
import Handler.User (userHandler)
import Handler.Post (postHandler)

server :: ServerT API AppM
server = authHandler
    :<|> userHandler
    :<|> postHandler

nt :: AppEnv -> AppM a -> Handler a
nt env appM = runReaderT appM env

app :: AppEnv -> Application
app env =
  serveWithContext apiProxy ctx $
    hoistServerWithContext apiProxy proxyCtx (nt env) server
  where
    ctx = defaultCookieSettings :. jwtSettings env :. EmptyContext
    proxyCtx = Proxy :: Proxy '[CookieSettings, JWTSettings]
