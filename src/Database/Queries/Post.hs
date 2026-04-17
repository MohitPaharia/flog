module Database.Queries.Post where

import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist
import Database.Persist.Postgresql

import Type.Post 
import Database.Schema

fetchAllPosts :: MonadIO m => SqlPersistT m [Entity Post]
fetchAllPosts = selectList [] []

fetchAllPostsOfUser :: MonadIO m => (Key User) -> SqlPersistT m [Entity Post]
fetchAllPostsOfUser uid = selectList [PostUser_id ==. uid] []

fetchPost :: MonadIO m => (Key Post) -> SqlPersistT m (Maybe Post)
fetchPost = get 

insertPost :: MonadIO m => (Key User) -> CreatePost -> SqlPersistT m (Entity Post)
insertPost userId  (CreatePost title content)  = do
  now <- liftIO getCurrentTime
  insertEntity $ Post
    {  postTitle      = title
    ,  postContent    = content
    ,  postUser_id    = userId
    ,  postCreated_at = now
    ,  postUpdated_at = now
    }

updatePost :: MonadIO m => (Key Post) -> UpdatePost -> SqlPersistT m Post
updatePost postId (UpdatePost mTitle mContent) = do
  now <- liftIO getCurrentTime
  updateGet postId $
    [PostUpdated_at =. now]
    ++ maybe [] (\t -> [PostTitle =. t]) mTitle
    ++ maybe [] (\c -> [PostContent =. c]) mContent 

deletePost :: MonadIO m => (Key Post) -> SqlPersistT m ()
deletePost = delete 
