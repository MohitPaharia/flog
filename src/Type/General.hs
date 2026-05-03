{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Type.General where

import           Data.Aeson           (FromJSON (parseJSON), ToJSON, withText)
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Database.Persist     (PersistField (..),
                                       PersistValue (PersistText),
                                       SqlType (SqlString))
import           Database.Persist.Sql (PersistFieldSql (..))
import           Text.Email.Validate  (isValid)
import           Web.HttpApiData      (FromHttpApiData (parseUrlPiece),
                                       ToHttpApiData (toUrlPiece))

data Zipper a = Zipper [a] a [a]
  deriving (Show, Eq)

instance Functor Zipper where
  fmap f (Zipper left x right) = Zipper left' x' right'
    where
      left'  = map f left
      x'     = f x
      right' = map f right

instance Applicative Zipper where
  pure x    = Zipper [] x []
  (<*>) (Zipper _ f _ ) z = f <$> z

focus :: Zipper a -> a
focus (Zipper _ x _) = x

inverse :: Zipper a -> Zipper a
inverse (Zipper left x right) = Zipper right x left

replace :: a -> Zipper a -> Zipper a
replace x' (Zipper left _ right ) = Zipper left x' right

mirror :: (Zipper a -> Zipper a) -> (Zipper a -> Zipper a)
mirror f = inverse . f . inverse

left :: a -> Zipper a -> a
left x' (Zipper left _ _) = case left of
  [] -> x'
  _  -> head left

right :: a -> Zipper a -> a
right x' = left x' . inverse

moveLeft :: Zipper a -> Zipper a
moveLeft z = case z of
  Zipper [] _ _              -> z
  Zipper (x' : left) x right -> Zipper left x' (x : right)

moveRight :: Zipper a -> Zipper a
moveRight = mirror moveLeft

shoveLeft :: a -> Zipper a -> Zipper a
shoveLeft x' (Zipper left x right) = Zipper ((reverse right) ++ (x: left)) x' []

shoveRight :: a -> Zipper a -> Zipper a
shoveRight x' = mirror $ shoveLeft x'

leftEnd :: Zipper a -> Zipper a
leftEnd z@(Zipper [] _ _)     = z
leftEnd (Zipper left x right) = Zipper [] (last left) ((reverse (init (left)))  ++ (x : right))

rightEnd :: Zipper a -> Zipper a
rightEnd = mirror leftEnd

pushLeft :: a -> Zipper a -> Zipper a
pushLeft x' (Zipper left x right) = Zipper (x : left) x' right

pushRight :: a -> Zipper a -> Zipper a
pushRight x' = mirror $ pushLeft x'

newtype Email = Email Text
  deriving (Show, Eq, ToJSON)

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

instance ToHttpApiData Email where
  toUrlPiece (Email e) = e
