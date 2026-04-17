{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type.User where

import GHC.Generics
import Data.Time (Day, UTCTime)
import Data.Aeson
import Data.Text (Text)

import Database.Schema
import Type.General (Email)

data CreateUser = CreateUser
  { name     :: Text
  , password :: Text
  , email    :: Email
  , dob      :: Maybe Day
  } deriving (Generic, Show)

instance ToJSON CreateUser
instance FromJSON CreateUser

data UpdateUser = UpdateUser
  { name :: Maybe Text
  , dob  :: Maybe Day  
  } deriving (Generic, Show)

instance ToJSON UpdateUser
instance FromJSON UpdateUser

data UserResponse = UserResponse
  { userId     :: UserId
  , name       :: Text
  , email      :: Email
  , dob        :: Maybe Day
  , created_at :: UTCTime
  } deriving (Generic, Show)

instance ToJSON UserResponse
instance FromJSON UserResponse
