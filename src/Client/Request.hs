{-# LANGUAGE TypeOperators #-}

module Client.Request where

import           Data.Int            (Int64)
import           Data.Text           (Text)
import           Servant
import           Servant.Auth.Client (Token)
import           Servant.Client      (ClientM, client)

import           API.Auth
import           API.Post
import           API.User
import           Type.Auth
import           Type.General
import           Type.Post
import           Type.User

login    :: Login -> ClientM TokenResponse
logout   :: Token -> ClientM NoContent
register :: CreateUser -> ClientM GenericResponse
verify   :: Maybe Email -> Maybe Text -> ClientM GenericResponse

login :<|> logout :<|> register :<|> verify = client (Proxy :: Proxy AuthAPI)

createUser :: CreateUser -> ClientM TokenResponse
getSelf    :: Token -> ClientM SelfResponse
getUsers   :: Token -> ClientM [UserResponse]
getUser    :: Token -> Int64 -> ClientM UserResponse
updateUser :: Token -> UpdateUser -> ClientM UserResponse
deleteUser :: Token -> ClientM NoContent

createUser :<|> getSelf :<|> getUsers :<|> getUser :<|> updateUser :<|> deleteUser = client (Proxy :: Proxy UserAPI)

getPostPreviews :: Token -> Int64 -> Maybe Int64 -> Maybe Int64 -> ClientM PostListResponse
getPost    :: Token -> Int64 -> Int64 -> ClientM PostResponse
createPost :: Token -> CreatePost -> ClientM PostResponse
updatePost :: Token -> Int64 -> UpdatePost -> ClientM PostResponse
deletePost :: Token -> Int64 -> ClientM NoContent

getPostPreviews :<|> getPost :<|> createPost :<|> updatePost :<|> deletePost = client (Proxy :: Proxy PostAPI)
