module API.OpenAPI where

import Servant
import Servant.OpenApi
import Data.OpenApi
import Data.Proxy

import API.API(apiProxy)

openapi :: OpenApi
openapi = undefined -- toOpenApi apiProxy
