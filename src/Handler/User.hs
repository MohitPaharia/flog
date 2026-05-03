{-# LANGUAGE DuplicateRecordFields #-}

module Handler.User (userHandler) where

import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Reader            (asks)
import qualified Data.ByteString                 as BL
import           Data.Int                        (Int64)
import           Data.Text.Encoding              (decodeUtf8)
import           Data.Time                       (addUTCTime, getCurrentTime)
import           Database.Esqueleto.Experimental (toSqlKey)
import           Database.Persist
import           Servant
import           Servant.Auth.Server

import           API.User                        (UserAPI)
import           App                             (AppM, jwtSettings,
                                                  requireAuth, runDB)
import           Database.Queries.Post           (postCountOfUser)
import           Database.Queries.User           (deleteUser, fetchAllUsers,
                                                  fetchUser, insertUser,
                                                  updateUser)
import           Type.Auth                       (AuthUser (..),
                                                  TokenResponse (..))
import           Type.Helper
import           Type.User                       (CreateUser (..), SelfResponse,
                                                  UpdateUser, UserResponse (..))

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

getUserH :: AuthResult AuthUser -> Int64 -> AppM UserResponse
getUserH auth userId = do
  _ <- requireAuth auth
  let key  = toSqlKey userId

  mUser <- runDB $ fetchUser key
  user  <- maybe (throwError err404) pure mUser

  pure $ toUserResponse (Entity key user)

getSelfH :: AuthResult AuthUser -> AppM SelfResponse
getSelfH auth = do
  AuthUser {userId = uid} <- requireAuth auth

  mUser <- runDB (fetchUser uid)
  user  <- maybe (throwError err404) pure mUser
  postCount <- runDB (postCountOfUser uid)

  pure $ toSelfResponse (Entity uid  user) postCount

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
