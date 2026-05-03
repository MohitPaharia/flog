{-# LANGUAGE TypeOperators #-}

module API.API(API, apiProxy) where

import           Servant

import           API.Auth (AuthAPI)
import           API.Post (PostAPI)
import           API.User (UserAPI)

type API = AuthAPI
 :<|> UserAPI
 :<|> PostAPI

apiProxy :: Proxy API
apiProxy = Proxy
