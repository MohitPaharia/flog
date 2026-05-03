{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.User where

import           Data.Int  (Int64)
import           Servant   as S

import           API.Auth  (Protected)
import           Type.Auth
import           Type.User

type UserAPI
    -- POST Create /users
       = "users" :> ReqBody '[JSON] CreateUser :> S.Post '[JSON] TokenResponse
    -- GET Show /self
    :<|> Protected :> "self" :> Get '[JSON] SelfResponse

    -- GET Index /users
    :<|> Protected :> "users" :> Get '[JSON] [UserResponse]

    -- GET Show /users/{id}
    :<|> Protected :> "users" :> Capture "id" Int64 :> Get '[JSON] UserResponse

    -- Patch update /users/
    :<|> Protected :> "users" :> ReqBody '[JSON] UpdateUser :> Patch '[JSON] UserResponse

    -- DELETE destroy /users/
    :<|> Protected :> "users" :> Delete '[JSON] NoContent


