{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server(app) where

import           Control.Monad.Reader

import           Servant
import           Servant.Auth.Server  (CookieSettings, JWTSettings,
                                       defaultCookieSettings)

import           API.API              (API, apiProxy)
import           App
import           Handler.Auth         (authHandler)
import           Handler.Post         (postHandler)
import           Handler.User         (userHandler)

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
