{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Post (PostAPI) where

import Servant as S

import API.Auth
import Type.Post
import Database.Schema as DB

type PostAPI =
  -- GET Index /users/{user_id}/posts
  Protected :> "users" :> Capture "user_id" (Key User) :> "posts" :> Get '[JSON] [PostResponse]  

  :<|>
  -- GET Show /users/{user_id}/posts/{id}
  Protected :> "users" :> Capture "user_id" (Key User) :> "posts" :> Capture "id" (Key DB.Post) :> Get '[JSON] PostResponse

  :<|>
  -- POST Create /posts
  Protected :> "posts" :> ReqBody '[JSON] CreatePost :> S.Post '[JSON] PostResponse

  :<|>
  -- Patch update /posts/{id}
  Protected :> "posts" :> Capture "id" (Key DB.Post) :> ReqBody '[JSON] UpdatePost :> Patch '[JSON] PostResponse

  :<|>
  -- DELETE destroy /posts/{id}
  Protected :> "posts" :> Capture "id" (Key DB.Post) :> Delete '[JSON] NoContent
  

