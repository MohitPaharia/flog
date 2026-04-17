module Handler.Post (postHandler) where

import Database.Persist
import Servant hiding (Post)
import Servant.Auth.Server

import App (AppM, runDB, requireAuth)
import API.Post (PostAPI)
import Database.Schema
import Database.Queries.Post
import Type.Auth (AuthUser(..))
import Type.Post (PostResponse, CreatePost, UpdatePost)
import Type.Helper (toPostResponse)
import Control.Monad.IO.Class (MonadIO(liftIO))

postHandler :: ServerT PostAPI AppM
postHandler = getAllPostsH
  :<|> getPostH 
  :<|> createPostH
  :<|> updatePostH 
  :<|> deletePostH

getAllPostsH :: AuthResult AuthUser -> (Key User) -> AppM [PostResponse]
getAllPostsH auth uid = do
  _ <- requireAuth auth
  posts <- runDB (fetchAllPostsOfUser uid)
  pure $ map toPostResponse posts

getPostH :: AuthResult AuthUser -> (Key User) -> (Key Post) -> AppM PostResponse
getPostH auth _ pid = do
  _ <- requireAuth auth
  mPost <- runDB (fetchPost pid)
  post <- maybe (throwError err404) pure mPost 
  pure $ toPostResponse (Entity pid post)

createPostH :: AuthResult AuthUser -> CreatePost -> AppM PostResponse
createPostH auth newPost = do
  AuthUser {userId = authId} <- requireAuth auth
  post <- runDB (insertPost authId newPost)
  pure $ toPostResponse post

updatePostH :: AuthResult AuthUser -> (Key Post) -> UpdatePost -> AppM PostResponse
updatePostH auth pid updates = do
  liftIO $ print updates 
  AuthUser {userId = authId} <- requireAuth auth
  mOldPost <- runDB (fetchPost pid)
  Post{postUser_id = authorId} <- maybe (throwError err404) pure mOldPost

  updatedPost <- if (authId /= authorId)
    then throwError err403
    else runDB (updatePost pid updates)

  pure $ toPostResponse (Entity pid updatedPost)

deletePostH :: AuthResult AuthUser -> (Key Post) -> AppM NoContent
deletePostH auth pid = do
  AuthUser {userId = authId} <- requireAuth auth
  mPost <- runDB (fetchPost pid)
  Post{postUser_id = authorId} <- maybe (throwError err404) pure mPost

  _ <- if (authId /= authorId)
    then (throwError err403)
    else runDB (deletePost pid)
  return NoContent
