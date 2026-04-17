{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.User where

import Servant as S

import API.Auth (Protected)
import Type.Auth
import Type.User
import Database.Schema as DB

-- POST Create /users
type UserAPI = "users" :> ReqBody '[JSON] CreateUser :> S.Post '[JSON] TokenResponse
      -- GET Show /self
    :<|> Protected :> "self" :> Get '[JSON] UserResponse

    -- GET Index /users
    :<|> Protected :> "users" :> Get '[JSON] [UserResponse]

    -- GET Show /users/{id}
    :<|> Protected :> "users" :> Capture "id" (Key User) :> Get '[JSON] UserResponse

    -- Patch update /users/
    :<|> Protected :> "users" :> ReqBody '[JSON] UpdateUser :> Patch '[JSON] UserResponse

    -- DELETE destroy /users/    
    :<|> Protected :> "users" :> Delete '[JSON] NoContent
  

