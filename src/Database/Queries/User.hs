module Database.Queries.User where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Text                   (Text)
import           Data.Time                   (Day, getCurrentTime)
import           Database.Persist
import           Database.Persist.Postgresql

import           Database.Schema
import           Type.General                (Email)
import           Type.User

insertUser :: MonadIO m => CreateUser -> SqlPersistT m (Maybe (Key User))
insertUser (CreateUser name password email dob) = do
  now <- liftIO getCurrentTime
  insertUnique $ User
    { userName       = name
    , userPassword   = password
    , userEmail      = email
    , userDob        = dob
    , userCreated_at = now
    }

-- searchUser :: MonadIO m => String -> SqlPersistT m (Maybe User)
-- searchUser =

fetchAllUsers :: MonadIO m => SqlPersistT m [Entity User]
fetchAllUsers = selectList [] []

fetchUser :: MonadIO m => Key User -> SqlPersistT m (Maybe User)
fetchUser = get

updateUser :: MonadIO m => Key User -> UpdateUser -> SqlPersistT m User
updateUser uid (UpdateUser newName newDob) = updateGet uid $
  maybe [] (\uname -> [UserName =. uname]) newName ++
  maybe [] (\dob -> [UserDob  =. newDob]) newDob

deleteUser :: MonadIO m => Key User -> SqlPersistT m ()
deleteUser = delete

checkUserExistsByEmail :: MonadIO m => Email -> SqlPersistT m Bool
checkUserExistsByEmail email =
  exists [UserEmail ==. email]

getUserByEmail :: MonadIO m => Email -> SqlPersistT m (Maybe (Entity User))
getUserByEmail email =
  selectFirst [UserEmail ==. email] []

getUserByEmailAndPassword :: MonadIO m => Email -> Text -> SqlPersistT m (Maybe (Entity User))
getUserByEmailAndPassword email password =
  selectFirst
    [ UserEmail ==. email
    , UserPassword ==. password
    ] []
