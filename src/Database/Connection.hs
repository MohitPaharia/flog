{-# LANGUAGE OverloadedStrings #-}

module Database.Connection (createPool) where

import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)

createPool :: ConnectionString -> Int -> IO ConnectionPool
createPool connStr connCount =
  runStdoutLoggingT $
    createPostgresqlPool connStr connCount
