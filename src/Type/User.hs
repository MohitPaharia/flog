{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type.User where

import           Data.Aeson
import           Data.Int     (Int64)
import           Data.Text    (Text)
import           Data.Time    (Day, UTCTime)
import           GHC.Generics

import           Type.General (Email)

data CreateUser = CreateUser
  { name     :: Text
  , password :: Text
  , email    :: Email
  , dob      :: Maybe Day
  } deriving (Generic, Show)

instance ToJSON   CreateUser
instance FromJSON CreateUser

data UpdateUser = UpdateUser
  { name :: Maybe Text
  , dob  :: Maybe Day
  } deriving (Generic, Show)

instance ToJSON   UpdateUser
instance FromJSON UpdateUser

data SelfResponse = SelfResponse
  { userId     :: Int64
  , name       :: Text
  , email      :: Email
  , dob        :: Maybe Day
  , postCount  :: Int
  , created_at :: UTCTime
  } deriving (Generic, Show)

instance ToJSON   SelfResponse
instance FromJSON SelfResponse

data UserResponse = UserResponse
  { userId     :: Int64
  , name       :: Text
  , email      :: Email
  , dob        :: Maybe Day
  , created_at :: UTCTime
  } deriving (Generic, Show)

instance ToJSON   UserResponse
instance FromJSON UserResponse
