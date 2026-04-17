{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type.Post where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

import Database.Schema

data CreatePost = CreatePost
  { title   :: Text 
  , content :: Text
  } deriving (Generic, Show)

instance FromJSON CreatePost

data UpdatePost = UpdatePost
  { newTitle   :: Maybe Text
  , newContent :: Maybe Text 
  } deriving (Generic, Show)

instance ToJSON UpdatePost
instance FromJSON UpdatePost

data PostResponse = PostResponse
  { postId     :: PostId
  , title      :: Text
  , content    :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  } deriving (Generic, Show)

instance ToJSON PostResponse
instance FromJSON PostResponse
