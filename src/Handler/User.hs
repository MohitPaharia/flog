{-# LANGUAGE DuplicateRecordFields #-}

module Handler.User (userHandler) where

import Servant
import Servant.Auth.Server
import Database.Persist
import qualified Data.ByteString as BL
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import Data.Time (addUTCTime, getCurrentTime)

import App (AppM, runDB, jwtSettings, requireAuth)
import API.User (UserAPI)
import Type.Helper
import Type.Auth (TokenResponse (..), AuthUser(..)) 
import Type.User (CreateUser(..), UserResponse (..), UpdateUser)
import Database.Queries.User (insertUser, fetchAllUsers, fetchUser, updateUser, deleteUser)
import Database.Schema (User)

userHandler :: ServerT UserAPI AppM
userHandler = createUserH 
  :<|> getSelfH
  :<|> getAllUsersH 
  :<|> getUserH
  :<|> updateUserH
  :<|> deleteUserH
  
createUserH ::  CreateUser -> AppM TokenResponse
createUserH usr@( CreateUser {email = e}) = do
  jwtSettings' <- asks jwtSettings 
  pKey <- maybe (throwError err409) pure =<< runDB (insertUser usr)

  let authUser = AuthUser {userId = pKey, email = e}
  now <- liftIO getCurrentTime
  let expiry = Just $ addUTCTime 3600 now -- 1 hour from now

  eJwt <- liftIO $ makeJWT authUser jwtSettings' expiry

  jwt <- case eJwt of
    Left _  -> throwError err500
    Right t -> pure t

  pure $ TokenResponse (decodeUtf8 $ BL.toStrict jwt)

getAllUsersH :: AuthResult AuthUser -> AppM [UserResponse]
getAllUsersH auth = do
  _ <- requireAuth auth
  users <- runDB (fetchAllUsers)
  pure $ map toUserResponse users

getUserH :: AuthResult AuthUser -> Key User -> AppM UserResponse
getUserH auth key = do
  _ <- requireAuth auth
  mUser <- runDB (fetchUser key)
  user  <- maybe (throwError err404) pure mUser

  pure $ toUserResponse (Entity key user)

getSelfH :: AuthResult AuthUser -> AppM UserResponse
getSelfH auth = do
  AuthUser {userId = uid} <- requireAuth auth

  mUser <- runDB (fetchUser uid)
  user  <- maybe (throwError err404) pure mUser

  pure $ toUserResponse (Entity uid  user)

updateUserH :: AuthResult AuthUser -> UpdateUser -> AppM UserResponse
updateUserH auth newData = do
  AuthUser {userId = uid} <- requireAuth auth

  user <- runDB (updateUser uid newData)

  pure $ toUserResponse (Entity uid user)

deleteUserH :: AuthResult AuthUser -> AppM NoContent
deleteUserH auth = do
  AuthUser {userId = uid} <- requireAuth auth
  runDB (deleteUser uid) 
  return NoContent
