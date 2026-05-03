{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Network.Wai.Handler.Warp

import           App                      (databaseConnectionPool,
                                           defaultEnvironment)
import           Database.Migration       (runMigrations)
import           Server                   (app)

main :: IO ()
main = do
  env <- defaultEnvironment
  runMigrations (databaseConnectionPool env)

  let settings =
        setPort 3000 $
        setHost "!6"  $
        defaultSettings

  putStrLn "Server is running at port: 3000"
  runSettings settings (app env)
