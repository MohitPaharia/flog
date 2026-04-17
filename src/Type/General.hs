{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.General where

import Text.Email.Validate (isValid)
import Data.Aeson
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist
import Database.Persist.Sql
import Web.HttpApiData

newtype Email = Email Text
  deriving (Show, Eq, ToJSON, Hashable)

mkEmail :: Text -> Maybe Email 
mkEmail t = if isValid (encodeUtf8  t)
  then Just $ Email t
  else Nothing
  
instance FromJSON Email where
  parseJSON = withText "Email" $ \t ->
    maybe (fail "Invalid email.") pure $ mkEmail t

instance FromHttpApiData Email where
  parseUrlPiece t = case (mkEmail t) of
    Just e  -> Right e
    Nothing -> Left "Invalid email."

-- Convert to/from DB
instance PersistField Email where
  toPersistValue (Email t) = PersistText t

  fromPersistValue (PersistText t) =
    case mkEmail t of
      Just e  -> Right e
      Nothing -> Left "Invalid email in database"

  fromPersistValue _ =
    Left "Expected PersistText for Email"

-- Tell SQL type
instance PersistFieldSql Email where
  sqlType _ = SqlString
