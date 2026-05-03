module Handler.Post (postHandler) where

import           Data.Int                        (Int64)
import           Database.Esqueleto.Experimental (Value (..), fromSqlKey,
                                                  toSqlKey)
import           Database.Persist
import           Servant                         hiding (Post)
import           Servant.Auth.Server

import           API.Post                        (PostAPI)
import           App                             (AppM, requireAuth, runDB)
import           Database.Queries.Post
import           Database.Schema
import           Type.Auth                       (AuthUser (..))
import           Type.Helper                     (toPostResponse)
import           Type.Post                       (CreatePost, PostListResponse,
                                                  PostPreview (..),
                                                  PostResponse, UpdatePost)

postHandler :: ServerT PostAPI AppM
postHandler = getPostPreviewsH
  :<|> getPostH
  :<|> createPostH
  :<|> updatePostH
  :<|> deletePostH

getPostPreviewsH :: AuthResult AuthUser -> Int64 -> Maybe Int64 -> Maybe Int64 -> AppM PostListResponse
getPostPreviewsH auth userId mLimit mOffset = do
  _ <- requireAuth auth

  let key = toSqlKey userId
  limit <- maybe (throwError err400) pure mLimit
  offset <- maybe (throwError err400) pure mOffset

  posts <- runDB (fetchPostPreview key limit offset)
  pure $ map
    (\(Value postId, Value title, Value createdAt) -> PostPreview (fromSqlKey postId) title createdAt) posts

getPostH :: AuthResult AuthUser -> Int64 -> Int64 -> AppM PostResponse
getPostH auth _ postId = do
  _ <- requireAuth auth
  let key = toSqlKey postId
  mPost <- runDB (fetchPost key)
  post <- maybe (throwError err404) pure mPost
  pure $ toPostResponse post

createPostH :: AuthResult AuthUser -> CreatePost -> AppM PostResponse
createPostH auth newPost = do
  AuthUser {userId = authId} <- requireAuth auth
  post <- runDB (insertPost authId newPost)
  pure $ toPostResponse post

updatePostH :: AuthResult AuthUser -> Int64 ->  UpdatePost -> AppM PostResponse
updatePostH auth postId updates = do
  AuthUser {userId = authId} <- requireAuth auth
  let key = toSqlKey postId

  mOldPost <- runDB (fetchPost key)
  Post{postUser_id = authorId} <-
    maybe
      (throwError err404)
      (pure . entityVal)
      mOldPost

  updatedPost <- if (authId /= authorId)
    then throwError err403
    else runDB (updatePost key updates)

  pure $ toPostResponse (Entity key updatedPost)

deletePostH :: AuthResult AuthUser -> Int64 -> AppM NoContent
deletePostH auth postId = do
  (AuthUser userId _) <- requireAuth auth
  let key = toSqlKey postId

  deletedRows <- runDB (deletePostOfUser userId key)

  if (deletedRows == 1)
    then return NoContent
    else throwError err403
