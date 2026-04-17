{-# LANGUAGE TypeOperators #-}

module API.API(API, apiProxy) where

import Servant

import API.Auth (AuthAPI)
import API.User (UserAPI)
import API.Post (PostAPI)

type API = AuthAPI
 :<|> UserAPI
 :<|> PostAPI

apiProxy :: Proxy API
apiProxy = Proxy 
