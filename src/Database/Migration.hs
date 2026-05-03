module Database.Migration(runMigrations) where

import           Control.Monad.Logger        (runStdoutLoggingT)
import           Database.Persist.Postgresql
import           Database.Schema

runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runStdoutLoggingT $
    flip runSqlPool pool $
      runMigration migrateAll
