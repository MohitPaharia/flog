{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type.Auth where

import Data.Aeson
import Data.Text (Text)
import Database.Persist (Entity(..))
import GHC.Generics
import Servant.Auth.Server

import Database.Schema
import Type.General

data Login = Login
  { email    :: Email
  , password :: Text
  } deriving (Generic, Show)

instance FromJSON Login
instance ToJSON Login

data AuthUser = AuthUser
  { userId :: UserId
  , email  :: Email
  } deriving (Generic, Show)

instance FromJSON AuthUser
instance ToJSON AuthUser
instance FromJWT AuthUser
instance ToJWT AuthUser

toAuthUser :: Entity User -> AuthUser
toAuthUser (Entity uid user) = AuthUser {userId = uid, email = userEmail user}

data GenericResponse = GenericResponse
  { message :: Text
  } deriving (Generic, Show)

instance FromJSON GenericResponse
instance ToJSON GenericResponse

data TokenResponse = TokenResponse
  { token :: Text
  } deriving (Generic, Show)

instance FromJSON TokenResponse
instance ToJSON TokenResponse


