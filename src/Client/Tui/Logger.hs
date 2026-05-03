{-# LANGUAGE OverloadedStrings #-}

module Client.Tui.Logger where

import           Data.Text             as T

import           Client.Tui.FileSystem (appendLog)
import           GHC.IO                (unsafePerformIO)

data LogLevel = INFO | TRACE | DEBUG | WARN | ERROR
  deriving (Show, Eq)

log :: LogLevel -> Text -> a -> a
log level txt expr = unsafePerformIO $ do
  appendLog (T.show level) txt
  return expr

