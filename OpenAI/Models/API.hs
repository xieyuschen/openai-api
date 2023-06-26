{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
module OpenAI.Models.API where
import Conf ( defaultDomain, fromDomain )
import Auth ( getAPIKey, getAPIKey' )

import qualified Data.ByteString.Lazy.Char8 as S8
import Data.ByteString as BS ()
import Data.ByteString.UTF8 as BSU ( fromString )
import Data.ByteString.Lazy.UTF8 as BLU ()
import Req ( baseRequest )
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
      defaultRequest, getResponseStatusCode, setRequestBearerAuth
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
import UnliftIO.Exception
import Colog (log, logError, logWarning,
    WithLog, Message, LogAction (unLogAction), logStringStdout, (<&), HasLog, richMessageAction,
    pattern D, pattern I, pattern W, LoggerT (runLoggerT), pattern E, (&>), logStringStderr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader ( MonadIO(liftIO) )
import Colog.Monad (HasLog(getLogAction, setLogAction))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Prelude hiding (log)
import Data.Text

listModels' :: WithLog env Message IO => ExceptT String IO ListResp
listModels' = do
  envE <- liftIO $ runExceptT getAPIKey'
  let Right envKey = envE
  let request
        = setRequestMethod "GET"
        $ setRequestBearerAuth (BSU.fromString envKey)
        $ setRequestPath "v1/models" baseRequest

  log D (append "the request sent by list models api are: " $ pack $ show request)
  manager <- liftIO $ newManager tlsManagerSettings
  errorOrResp <- liftIO $ tryAny $ httpLbs request manager
  case errorOrResp of
    Left e -> do
      log E $ append "fail to send request: " $ pack $ show e
      fail "fail to send request"
    Right response -> do
      if getResponseStatusCode response == 200
        then do
          let dataM = Data.Aeson.decode $ responseBody response
          case dataM of
            Nothing -> do
              log E $ append "non-200 response: " $ pack $ show $ getResponseStatusCode response
              fail "non-200 response"
            Just model -> 
              return model
        else do
          log E $ append "non-200 response: " $ pack $ show $ getResponseStatusCode response
          fail "non-200 response"



listModels :: IO (Maybe ListResp)
listModels = do
  ex <- runExceptT getAPIKey'
  let Right key = ex
  let request
          = setRequestMethod "GET"
          $ setRequestBearerAuth (BSU.fromString key)
          $ setRequestPath "v1/models" baseRequest 
  manager <- liftIO $ newManager tlsManagerSettings
  errorOrResp <- liftIO $ tryAny $ httpLbs request manager
  
  case errorOrResp of
    Left e -> do
      logStringStderr <& ("Error: fail to send request " ++ show e)
      return Nothing
    Right response -> do
      if getResponseStatusCode response == 200
        then do
          let bodyStr = responseBody response
          return $ Data.Aeson.decode bodyStr
        else do
          logStringStdout <& ("Error: " ++ show (getResponseStatusCode response) ++ " " ++ show (responseBody response))
          return Nothing

retrieveModel :: String -> IO (Maybe Model)
retrieveModel modelID = do
  ex <- runExceptT getAPIKey'
  let Right key = ex
  let request
          = setRequestMethod "GET"
          $ setRequestBearerAuth (BSU.fromString key)          
          $ setRequestPath (BSU.fromString("v1/models/" ++ modelID)) baseRequest 

  manager <- liftIO $ newManager tlsManagerSettings
  errorOrResp <- liftIO $ tryAny $ httpLbs request manager
  
  case errorOrResp of
    Left e -> do
      logStringStderr <& ("Error: fail to send request " ++ show e)
      return Nothing
    Right response -> do
      if getResponseStatusCode response == 200
        then do
          let bodyStr = responseBody response
          return $ Data.Aeson.decode bodyStr
        else do
          logStringStdout <& ("Error: " ++ show (getResponseStatusCode response) ++ " " ++ show (responseBody response))
          return Nothing 