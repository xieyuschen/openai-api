{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenAI.Models.API where
import Conf ( defaultDomain, fromDomain )
import Auth ( getAPIKey )

import qualified Data.ByteString.Lazy.Char8 as S8
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU
import Data.ByteString.Lazy.UTF8 as BLU

import Network.HTTP.Simple
    ( getResponseBody
      , getResponseHeader
      , getResponseStatus
      , httpJSON
      , setRequestMethod
      , setRequestPath
      , setRequestSecure
      , setRequestPort
      , setRequestHost
      , Request
      , Response, setRequestHeaders,
      defaultRequest, getResponseStatusCode
      )
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Client        (defaultManagerSettings, newManager, withResponse, httpLbs, Response (responseBody))
import           Network.HTTP.Types.Status   (statusCode)
import Data.Aeson.Types
    ( (.:),
      FromJSON(parseJSON),
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON),
      typeMismatch )
import Control.Monad.Trans.Maybe
import OpenAI.Models.Data
import Data.Aeson (decode)
import qualified Codec.Binary.UTF8.Generic as BS
import Data.IORef
import UnliftIO.Exception

-- list :: MaybeT IO ListResp
-- todo: refine the API to a better one
listModels :: IO (Maybe ListResp)
listModels = do
  eitherKey <- getAPIKey

  let Right key = eitherKey
  let request
          = setRequestMethod "GET"
          $ setRequestHost (BSU.fromString $ fromDomain defaultDomain)
          $ setRequestSecure True
          $ setRequestHeaders [("Content-Type", "application/json"),
            ("Authorization", BSU.fromString $ "Bearer " ++ key) ]
          $ setRequestPort 443
          $ setRequestPath "v1/models" defaultRequest

  manager <- newManager tlsManagerSettings
  errorOrResp <- tryAny $ httpLbs request manager
  case errorOrResp of
    Left e -> do
      print e
      return Nothing
    Right response -> do
      if getResponseStatusCode response == 200
        then do
          let bodyStr = responseBody response
          return $ Data.Aeson.decode bodyStr
        else return Nothing

