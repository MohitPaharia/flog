{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql 
import Servant 
import Servant.Auth.Server

import Database.Connection (createPool)

data AppEnv = AppEnv
  { databaseConnectionPool :: ConnectionPool
  , jwtSettings            :: JWTSettings
  } 

connStr :: ConnectionString
connStr = "postgres://marcus:aurelius@localhost/flog_db"

defaultEnvironment :: IO AppEnv
defaultEnvironment = do
  connPool <- createPool connStr 10
  jwtKey   <- generateKey
  let jwt = defaultJWTSettings jwtKey

  pure AppEnv
    { databaseConnectionPool = connPool
    , jwtSettings            = jwt
    }

type AppM = ReaderT AppEnv Handler

runDB :: SqlPersistT IO a -> AppM a
runDB query = do
  pool <- asks databaseConnectionPool
  liftIO $ runSqlPool query pool

requireAuth :: AuthResult a -> AppM a
requireAuth (Authenticated a) = pure a
requireAuth _ = throwError err401
