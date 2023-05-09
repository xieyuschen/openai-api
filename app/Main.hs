{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main where
import Conf ( defaultDomain, fromDomain )
import Auth ( getAPIKey )

import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.Yaml             as Yaml
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU  ( fromString )
import Network.HTTP.Simple
    ( getResponseBody
      , getResponseHeader
      , getResponseStatusCode
      , httpJSON
      , setRequestMethod
      , setRequestPath
      , setRequestSecure
      , setRequestPort
      , setRequestHost
      , Request
      , Response, setRequestHeaders
      )
import Network.HTTP.Client (defaultRequest)
import           Data.Aeson (Value )

main :: IO ()
main = do
  eitherKey <- getAPIKey

  let Right key = eitherKey
  
  let request
          = setRequestMethod "GET"
          $ setRequestHost (fromString $ fromDomain defaultDomain)
          $ setRequestSecure True
          $ setRequestHeaders [("Content-Type", "application/json"),
            ("Authentication", fromString $ "Bearer " ++ key) ]
          $ setRequestPort 443
          $ setRequestPath "v1/models" defaultRequest
  -- https://stackoverflow.com/questions/46694290/why-does-httpjson-fail-but-httplbs-succeeds
  -- `response <- httpJSON` request doesn't work
  response <- httpJSON request :: IO ( Response ()) 

  putStrLn $ "The status code was: " ++
              show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response