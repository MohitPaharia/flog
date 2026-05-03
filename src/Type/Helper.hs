{-# LANGUAGE DuplicateRecordFields #-}

module Type.Helper where

import           Database.Esqueleto.Experimental (fromSqlKey)
import           Database.Persist

import           Database.Schema
import           Type.Post
import           Type.User

toSelfResponse :: Entity User -> Int -> SelfResponse
toSelfResponse (Entity uid usr) postCount' = SelfResponse
  { userId     = fromSqlKey uid
  , name       = userName usr
  , email      = userEmail usr
  , dob        = userDob usr
  , postCount  = postCount'
  , created_at = userCreated_at usr
  }

toUserResponse :: Entity User -> UserResponse
toUserResponse (Entity uid usr) = UserResponse
  { userId     = fromSqlKey uid
  , name       = userName usr
  , email      = userEmail usr
  , dob        = userDob usr
  , created_at = userCreated_at usr
  }

toPostResponse :: Entity Post -> PostResponse
toPostResponse (Entity pid post) = PostResponse
  { postId     = fromSqlKey pid
  , title      = postTitle post
  , content    = postContent post
  , created_at = postCreated_at post
  , updated_at = postUpdated_at post
  }
