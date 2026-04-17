{-# LANGUAGE DuplicateRecordFields #-}

module Type.Helper where

import Database.Persist

import Database.Schema
import Type.User (UserResponse(..))
import Type.Post (PostResponse(..))

toUserResponse :: Entity User -> UserResponse
toUserResponse (Entity uid usr) = UserResponse
  { userId     = uid
  , name       = userName  usr
  , email      = userEmail usr
  , dob        = userDob usr
  , created_at = userCreated_at usr
  }

toPostResponse :: Entity Post -> PostResponse
toPostResponse (Entity pid post) = PostResponse
  { postId     = pid
  , title      = postTitle post
  , content    = postContent post
  , created_at = postCreated_at post
  , updated_at = postUpdated_at post 
  }
