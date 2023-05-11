{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module OpenAI.Completions.API where
import Conf ( defaultDomain, fromDomain )
import Auth ( getAPIKey, getAPIKey' )

import qualified Data.ByteString.Lazy.Char8 as S8
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU
import Data.ByteString.Lazy.UTF8 as BLU
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
      defaultRequest, getResponseStatusCode, setRequestBearerAuth, setRequestBody, setRequestBodyJSON
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
import Control.Monad.Except (runExceptT)
import OpenAI.Chat.Data ( ChatRequest (ChatRequest), Message (Message), exampleJson, ChatCompletion )
import OpenAI.Completions.Data (ReqCompletions, Completions)

completions :: ReqCompletions -> IO (Maybe Completions)
completions req = do
  ex <- runExceptT getAPIKey'
  let Right key = ex
  let request
          = setRequestMethod "POST"
          $ setRequestBearerAuth (BSU.fromString key)          
          $ setRequestPath (BSU.fromString "v1/completions")
          $ setRequestBodyJSON req baseRequest 

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