module Database.Migration(runMigrations) where

import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Schema

runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runStdoutLoggingT $
    flip runSqlPool pool $
      runMigration migrateAll
