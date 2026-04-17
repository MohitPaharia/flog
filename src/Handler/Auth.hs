{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Handler.Auth(authHandler) where

import Control.Monad (when, replicateM)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import qualified Data.ByteString as BL
import Data.Cache as Cache
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist
import Servant
import Servant.Auth.Server
import System.Random (randomRIO)

import API.Auth (AuthAPI)
import App
import Database.Schema
import Database.Queries.TempUser (insertTempUser, deleteTempUserByEmail, getTempUserByEmail, deleteTempUser)
import Database.Queries.User
import Mail
import Type.Auth
import Type.General (Email)
import Type.User (CreateUser(..))

authHandler :: ServerT AuthAPI AppM
authHandler = loginH
         :<|> processRegistrationH
         :<|> verifyTokenH
 
loginH :: Login -> AppM TokenResponse
loginH (Login email password) = do
  jwtSettings' <- asks jwtSettings 
  user <- maybe (throwError err404) pure
      =<< runDB (getUserByEmailAndPassword email password)

  now <- liftIO getCurrentTime
  let expiry   = addUTCTime 3600 now -- 1 hour from now
  let authUser = toAuthUser user

  eJwt <- liftIO $ makeJWT authUser jwtSettings' (Just expiry) 

  jwt <- case eJwt of
    Left _ -> throwError err500
    Right t -> pure t

  pure $ TokenResponse $ decodeUtf8 $ BL.toStrict jwt

processRegistrationH :: CreateUser -> AppM GenericResponse
processRegistrationH newUser@(CreateUser name password email dob) = do
  -- Check if email is already in use by a registered user.
  emailAlreadyExists <- runDB $ checkUserExistsByEmail email
  when emailAlreadyExists $ throwError err409 
  
  mOldTempUser <- runDB $ getTempUserByEmail email
  when (isJust mOldTempUser) $ throwError err401
 
  now <- liftIO $ getCurrentTime
  let expiry_time =  addUTCTime (600) now -- 10 minutes from now 
  token <- liftIO generateToken

  mKey <- runDB $ insertTempUser token expiry_time newUser
  when (isNothing mKey) $ throwError err401

  success <- liftIO $ sendVerificationMail Mock email token

  if success 
    then pure $ GenericResponse { message = "Check your mail." }
    else do
      runDB (deleteTempUserByEmail email)
      throwError err500

generateToken :: IO T.Text
generateToken = T.pack <$> replicateM 8 (randomRIO ('0', '9')) 

verifyTokenH :: Maybe Email -> Maybe T.Text -> AppM GenericResponse
verifyTokenH mEmail mToken = do
  email <- maybe (throwError err400) pure mEmail
  token <- maybe (throwError err400) pure mToken
  
  mTempUser <- runDB $ getTempUserByEmail email
  (Entity key user)  <- maybe (throwError err401) pure mTempUser

  now <- liftIO getCurrentTime

  when (now > tempUserExpired_at user) $ do
    runDB $ deleteTempUser key
    throwError err401

  when (tempUserToken user /= token) $ throwError err401
  runDB $ deleteTempUser key

  let newUser  = CreateUser {name = tempUserName user
    , password = tempUserPassword user
    , email    = tempUserEmail user
    , dob      = tempUserDob user
    }

  mKey <- runDB $ insertUser newUser
  when (isNothing mKey) $ throwError err401

  pure $ GenericResponse { message = "Registration Successfull." }
