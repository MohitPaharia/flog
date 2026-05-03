{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type.Post where

import           Data.Aeson
import           Data.Int     (Int64)
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

data CreatePost = CreatePost
  { title   :: Text
  , content :: Text
  } deriving (Generic, Show)

instance ToJSON   CreatePost
instance FromJSON CreatePost

data UpdatePost = UpdatePost
  { newTitle   :: Maybe Text
  , newContent :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON   UpdatePost
instance FromJSON UpdatePost

data PostResponse = PostResponse
  { postId     :: Int64
  , title      :: Text
  , content    :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  } deriving (Generic, Show)

instance ToJSON   PostResponse
instance FromJSON PostResponse

data PostPreview  = PostPreview
  { postId     :: Int64
  , title      :: Text
  , created_at :: UTCTime
  } deriving (Generic, Show)

instance ToJSON PostPreview
instance FromJSON PostPreview

type PostListResponse = [PostPreview]

