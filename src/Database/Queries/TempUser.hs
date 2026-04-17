module Database.Queries.TempUser where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Postgresql (SqlPersistT)

import Database.Schema
import Type.General (Email)
import Type.User (CreateUser(..))
 
insertTempUser :: MonadIO m =>  Text -> UTCTime-> CreateUser -> SqlPersistT m (Maybe (Key TempUser))
insertTempUser token expiryTime(CreateUser name password email dob) = 
  insertUnique $ TempUser
    { tempUserEmail      = email
    , tempUserName       = name
    , tempUserPassword   = password
    , tempUserDob        = dob
    , tempUserExpired_at = expiryTime
    , tempUserToken      = token
    }


getTempUserByEmail :: MonadIO m => Email -> SqlPersistT m (Maybe (Entity TempUser))
getTempUserByEmail email = 
  selectFirst [TempUserEmail ==. email] []

deleteTempUserByEmail :: MonadIO m => Email -> SqlPersistT m ()
deleteTempUserByEmail email = deleteWhere [TempUserEmail ==. email]

deleteTempUser :: MonadIO m => (Key TempUser) -> SqlPersistT m ()
deleteTempUser = delete
