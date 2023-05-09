{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module OpenAI.Models.API where
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

import OpenAI.Models.Data

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

  manager <- newManager tlsManagerSettings
  withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                show (statusCode $ getResponseStatus response)
        value <- bodyReaderSource (getResponseBody response)
              $$ sinkParser json
        
          case decode json of
            Just (ListResp _ models) -> do
              putStrLn "Models:"
              mapM_ (putStrLn . modelToString) models
            _ -> putStrLn "Error parsing JSON"