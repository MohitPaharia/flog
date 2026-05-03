{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Tui.Command where

import           Brick
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Text                       as T
import qualified Data.Text.Read                  as TR
import           Database.Esqueleto.Experimental (toSqlKey)
import           Servant.Client                  (runClientM)

import qualified Client.Request                  as Req
import           Client.Tui.FileSystem           (removeToken, storeToken)
import           Client.Tui.Helper
import qualified Client.Tui.Kaomoji              as Moji
import           Client.Tui.Logger               as L
import           Client.Tui.Type
import           Type.Auth
import           Type.General

noCmd :: Command
noCmd = Command
  { name         = ""
  , usage        = "Press ESC to exit command mode."
  , description  = ""
  , mascot       = Moji.happy
  , argsResolver = \_ -> Right ()
  , execute      = \_ -> pure ()
  }

unknown :: Command
unknown = Command
  { name         = "Unknown"
  , usage        = "Invalid command."
  , description  = ""
  , mascot       = Moji.idk
  , argsResolver = \_ -> Left "Invalid Command"
  , execute      = \_ -> pure ()
  }

quit :: Command
quit = Command
  { name         = "quit"
  , usage        = ":q, :quit"
  , description  = "Exit the application."
  , mascot       = Moji.sad
  , argsResolver = \case
      [] -> Right ()
      _  -> Left "This command don't take any argument."
  , execute      = \_ -> halt
  }

help :: Command
help = Command
 { name         = "help"
 , usage        = ":h <command>, :help <command>"
 , description  = "Show the details of a commands."
 , mascot       = Moji.smiling
 , argsResolver = \case
     [cmd] -> Right cmd
     _     -> Left "This command take one argument."

 , execute      = \cmd -> do
     let (cmd', _) = parseCmd cmd
     popupMode "Command" . T.pack $ show cmd'

 }

login :: Command
login = Command
  { name         = "login"
  , usage        = ":login <email> <password>"
  , description  = "Login into your flog account."
  , mascot       = Moji.observer
  , argsResolver = \case
      [mail, passwd] ->
        case mkEmail $ mail of
          Just e  -> Right (e, passwd)
          Nothing -> Left $ "Invalid email."

      _ -> Left "Expected Args: <email> <password>"

  , execute      = \(email, password) -> do
      st <- get

      let env'  = env st
          param = Login email password


      res <- liftIO $ runClientM (Req.login param) env'
      case res of
        Left err -> errMode $ T.show err
        Right (TokenResponse { token = t }) -> do
          t' <- liftIO $ storeToken t
          modify (\s -> s { mode = Popup "Message" "Login successful!", jwt = Just t' })
  }

logout :: Command
logout = Command
  { name  = "logout"
  , usage = ":logout"
  , description = "Logout from flog account."
  , mascot = Moji.crying
  , argsResolver = \case
      [] -> Right ()
      _  -> Left "No arguments expected."

  , execute = \_ -> do
      st <- get

      let jwt' = jwt st
          env' = env st

      case jwt' of
        Nothing -> errLogin
        Just t  -> do
          liftIO $ removeToken
          liftIO $ runClientM (Req.logout t) env'

          modify (\s -> s { mode = View, page = Home, jwt = Nothing })
  }

self :: Command
self = Command
  { name  = "self"
  , usage = ":self"
  , description = "Show flog account profile."
  , mascot = Moji.sideEye
  , argsResolver = \case
      [] -> Right ()
      _  -> Left "This command don't take any argument."

  , execute = \_ -> do
      st <- get
      let env' = env st
          t    = jwt st

      case t of
        Nothing -> errLogin
        Just t' -> do
          res  <- liftIO $ runClientM (Req.getSelf t') env'

          case res of
            Left err -> errMode $ T.show err
            Right dt -> modify (\s -> s { mode = View, page = Self_' dt })
  }

postOf :: Command
postOf = Command
  { name        = "post-of"
  , usage       = ":post-of <user-id> <post-id>"
  , description = "Used to the content of post."
  , mascot = Moji.observer
  , argsResolver = \case
      [userId, postId] -> case (userId', postId') of
          (Just uid, Just pid) -> Right (uid, pid)
          _                    -> Left "Invalid ID."
        where
          userId' = case TR.decimal userId of
            Right (uid, "") -> Just uid
            _               -> Nothing

          postId' = case TR.decimal postId of
            Right (pid, "") -> Just pid
            _               -> Nothing

      _ -> Left "This command take only one argument."

  , execute = \(userId, postId) -> do
      st <- get
      let env' = env st
          t    = jwt st

      case t of
        Nothing -> errLogin
        Just t' -> do
          res <- liftIO $ runClientM (Req.getPost t' userId postId) env'

          case res of
            Left err   -> errMode $ T.show err
            Right postResponse -> modify (\s -> s { mode = View, page = Post_' postResponse })
  }

postsFrom :: Command
postsFrom = Command
  { name = "posts-from"
  , usage = ":posts-from <user-id> :posts-from <user-id> <page-no>"
  , description = "Show the list of posts from a specific user."
  , mascot = Moji.sideEye
  , argsResolver = \case
      [userId] -> case TR.decimal userId of
        Right (userId', "") -> Right userId'
        _                   -> Left "Invalid ID"
      _ -> Left "This command take only one argument."
  , execute = \(userId) -> do
      st <- get
      let env' = env st
          t    = jwt st

      case t of
        Nothing  -> errLogin
        Just t' -> do
          res <- liftIO $ runClientM (Req.getPostPreviews t' userId (Just 10) (Just 1)) env'

          case res of
            Left err -> errMode $ T.show err
            Right postListResponse -> modify (\s -> s { mode = View, page = PostPreview_' postListResponse})
  }

-- Common error user might get after executing a command
errLogin :: EventM () St ()
errLogin = modify (\s -> s { mode = Error "Login first." })


-- Command Parser
parseCmd :: T.Text -> (Command, [T.Text])
parseCmd input = (cmd', args)
  where
    (cmd, args) = case T.words input of
      (x: xs) -> (x, xs)
      []      -> ("", [])

    cmd' = case cmd of
      "q"          -> quit
      "quit"       -> quit
      "h"          -> help
      "help"       -> help
      "login"      -> login
      "logout"     -> logout
      "self"       -> self
      "post-of"    -> postOf
      "posts-from" -> postsFrom
      ""           -> noCmd
      _            -> unknown
