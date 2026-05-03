{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API.Post (PostAPI) where

import           Data.Int  (Int64)
import           Servant   as S

import           API.Auth
import           Type.Post

type PostAPI =
  -- GET Index /users/{user_id}/posts
  Protected :> "users" :> Capture "user_id" Int64 :> "posts"
            :> QueryParam "limit" Int64 :> QueryParam "offset" Int64 :>  Get '[JSON] PostListResponse

  :<|>
  -- GET Show /users/{user_id}/posts/{id}
  Protected :> "users" :> Capture "user_id" Int64 :> "posts" :> Capture "post_id" Int64 :> Get '[JSON] PostResponse

  :<|>
  -- POST Create /posts
  Protected :> "posts" :> ReqBody '[JSON] CreatePost :> S.Post '[JSON] PostResponse

  :<|>
  -- Patch update /posts/{id}
  Protected :> "posts" :> Capture "post_id" Int64 :> ReqBody '[JSON] UpdatePost :> Patch '[JSON] PostResponse

  :<|>
  -- DELETE destroy /posts/{id}
  Protected :> "posts" :> Capture "post_id" Int64 :> Delete '[JSON] NoContent


