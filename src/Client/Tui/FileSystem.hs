{-# LANGUAGE OverloadedStrings #-}

module Client.Tui.FileSystem where

import           Client.Tui.Type     (CmdHistory)
import           Control.Monad       (when)
import           Data.List.Extra     (takeEnd)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Text.IO        as TIO
import           Data.Time           (getCurrentTime)
import           Servant.Auth.Client (Token (..))
import           System.Directory    (createDirectoryIfMissing, doesFileExist,
                                      getHomeDirectory, removeFile)
import           System.FilePath     ((</>))
import           Type.General

appDir :: IO FilePath
appDir = do
  home <- getHomeDirectory
  pure $ home </> ".cache" </> "flog"

tokenFile :: IO FilePath
tokenFile = do
  dir <- appDir
  pure $ dir </> "token.txt"

historyFile :: IO FilePath
historyFile = do
  dir <- appDir
  pure $ dir </> "history.txt"

logFile :: IO FilePath
logFile = do
  dir <- appDir
  pure $ dir </> "app.log"

storeToken :: T.Text -> IO Token
storeToken token = do
  dir      <- appDir
  filePath <- tokenFile

  createDirectoryIfMissing True dir
  TIO.writeFile filePath token
  return . Token $ TE.encodeUtf8 token

removeToken :: IO ()
removeToken = do
  filePath <- tokenFile
  exists <- doesFileExist filePath

  when exists $ removeFile filePath

  return ()

getToken :: IO (Maybe Token)
getToken = do
  filePath <- tokenFile
  exists   <- doesFileExist filePath

  if exists
    then Just . Token . TE.encodeUtf8 <$> (TIO.readFile $ filePath)
    else pure $ Nothing

loadHistory :: Int -> IO CmdHistory
loadHistory limit = do
  filePath <- historyFile
  exists   <- doesFileExist filePath

  if exists
    then do
      history <- reverse . takeEnd limit . T.lines <$> TIO.readFile filePath
      pure $ Zipper history "" []
    else pure $ Zipper [] "" []

resizeHistory :: Int -> IO ()
resizeHistory limit = do
  filePath <- historyFile
  exists   <- doesFileExist filePath

  if exists
    then do
      history <- takeEnd limit . T.lines <$> TIO.readFile filePath
      TIO.writeFile filePath $ (T.intercalate "\n" history) <> "\n"
    else pure ()


updateHistory :: T.Text -> IO ()
updateHistory cmd = do
  dir      <- appDir
  filePath <- historyFile

  createDirectoryIfMissing True dir
  TIO.appendFile filePath $ cmd <> "\n"

appendLog :: T.Text -> T.Text -> IO ()
appendLog tag message = do
  dir      <- appDir
  filePath <- logFile
  currentTime <- getCurrentTime
  let time =  surround $ T.show currentTime
      tag' =  surround tag

  createDirectoryIfMissing True dir
  TIO.appendFile filePath $ T.intercalate " " [time, tag', message] <> "\n"

  where surround t = "[" <> t <> "]"
