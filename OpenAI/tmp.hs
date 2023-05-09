{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module Tmp where


import Conf ( defaultDomain, fromDomain )
import Auth ( getAPIKey )

import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.Yaml             as Yaml
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU  ( fromString )
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
      defaultRequest
      )

import Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Client        (defaultManagerSettings, newManager, withResponse)
import           Data.Aeson.Parser           (json, value)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Types.Status   (statusCode)
import Network.HTTP.Client.Conduit(bodyReaderSource)
import Data.Aeson.Types
    ( (.:),
      FromJSON(parseJSON),
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON),
      typeMismatch )

fn :: IO ()
fn = do
  eitherKey <- getAPIKey

  let Right key = eitherKey
  
  let request
          = setRequestMethod "GET"
          $ setRequestHost (fromString $ fromDomain defaultDomain)
          $ setRequestSecure True
          $ setRequestHeaders [("Content-Type", "application/json"),
            ("Authorization", fromString $ "Bearer " ++ key) ]
          $ setRequestPort 443
          $ setRequestPath "v1/models" defaultRequest
  -- https://stackoverflow.com/questions/46694290/why-does-httpjson-fail-but-httplbs-succeeds
  -- `response <- httpJSON` request doesn't work

  manager <- newManager tlsManagerSettings
  withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                show (statusCode $ getResponseStatus response)
        value <- bodyReaderSource (getResponseBody response)
              $$ sinkParser json
        print value