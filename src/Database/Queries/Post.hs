{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Database.Queries.Post where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Int                        (Int64)
import           Data.Text                       (Text)
import           Data.Time                       (UTCTime, getCurrentTime)
import           Database.Esqueleto.Experimental hiding (delete, (=.))
import           Database.Persist                hiding ((==.))

import           Database.Schema
import           Type.Post

fetchAllPosts :: MonadIO m => SqlPersistT m [Entity Post]
fetchAllPosts = select $ do
  posts <- from $ table @Post
  pure posts

fetchAllPostsOfUser :: MonadIO m => (Key User) -> SqlPersistT m [Entity Post]
fetchAllPostsOfUser userId = select $ do
  posts <-  from $ table @Post
  where_ (posts ^. PostUser_id ==. val userId)
  pure posts

fetchPost :: MonadIO m => (Key Post) -> SqlPersistT m (Maybe (Entity Post))
fetchPost postId = selectOne $ do
    post <- from $ table @Post
    where_ (post ^. PostId ==. val postId)
    pure post

fetchPostPreview :: MonadIO m => (Key User) -> Int64 -> Int64 -> SqlPersistT m [(Value (Key Post), Value Text, Value UTCTime)]
fetchPostPreview userId limit' offset' =
  select $  do
    post <- from $ table @Post
    where_ (post ^. PostUser_id ==. val userId)
    orderBy [desc (post ^. PostCreated_at)]
    limit limit'
    offset offset'
    pure (post ^. PostId, post ^. PostTitle, post ^. PostCreated_at)

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

deletePostOfUser :: MonadIO m => (Key User) -> (Key Post) -> SqlPersistT m Int64
deletePostOfUser userId postId = deleteCount $ do
    post <- from $ table @Post
    where_ (post ^. PostUser_id ==. val userId &&. post ^. PostId ==. val postId)

postCountOfUser :: MonadIO m => (Key User) -> SqlPersistT m Int
postCountOfUser userId = do
  result <- select $ do
    posts <- from $ table @Post

    where_ (posts ^. PostUser_id ==. val userId)

    pure (countRows @Int)

  pure $
    case result of
      [Value n] -> n
      _         -> 0
