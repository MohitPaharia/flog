{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth (AuthAPI, Protected) where

import Servant
import Servant.Auth
import Data.Text (Text)

import Type.Auth
import Type.General (Email)
import Type.User

type AuthAPI =
-- /login
  "login" :> ReqBody '[JSON] Login :> Post '[JSON] TokenResponse

-- /register
  :<|> "register" :> ReqBody '[JSON] CreateUser :> Post '[JSON] GenericResponse

-- /register/
  :<|> "register" :> "verify" :> QueryParam "email" Email :> QueryParam "token" Text :> Get '[JSON] GenericResponse
   
type Protected = Auth '[JWT] AuthUser
