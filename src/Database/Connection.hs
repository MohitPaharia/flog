{-# LANGUAGE OverloadedStrings #-}

module Database.Connection (createPool) where

import           Control.Monad.Logger        (runStdoutLoggingT)
import           Database.Persist.Postgresql

createPool :: ConnectionString -> Int -> IO ConnectionPool
createPool connStr connCount =
  runStdoutLoggingT $
    createPostgresqlPool connStr connCount
