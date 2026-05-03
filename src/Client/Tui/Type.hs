{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Tui.Type where

import           Brick               (EventM)
import           Data.Text           as T
import           Data.Time           (Day)
import           Servant.Auth.Client (Token (..))
import           Servant.Client      (ClientEnv)

import           Type.General        (Zipper)
import           Type.Post           (PostListResponse, PostResponse)
import           Type.User           (SelfResponse, UserResponse)

data Command where
  Command ::
    { name         :: Text
    , usage        :: Text
    , description  :: Text
    , argsResolver :: [Text] -> Either Text args
    , execute      :: args -> EventM () St ()
    , mascot       :: Text
    }
    -> Command

instance Show Command where
  show (Command n u d _ _ m) =
    T.unpack $ T.intercalate "\n"
      ["Name: " <> n, "Usage: " <> u, "Description: " <> d, "Mascot: " <> m]

data Mode = Cli Command
          | View
          | Popup Text Text
          | Error Text

data Page = Home
          | Self_'        SelfResponse
          | Post_'        PostResponse
          | PostPreview_' PostListResponse

type CmdHistory = Zipper Text

data St = St
  { mode        :: Mode
  , page        :: Page
  , jwt         :: Maybe Token
  , env         :: ClientEnv
  , cmdHistory  :: CmdHistory
  , currentDate :: Day
  }
