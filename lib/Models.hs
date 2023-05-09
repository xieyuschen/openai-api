{-# LANGUAGE OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}

module Models where
import Conf ( defaultDomain, fromDomain )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
    ( getResponseBody,
      getResponseHeader,
      getResponseStatusCode,
      httpLBS )


-- main :: IO ()
-- main = do
--     request' <- parseRequest "POST http://httpbin.org/post"
--     let request = setRequestMethod "GET"
--         $ setRequestHost $ fromDomain defaultDomain
--         $ request'
    
--     response <- httpJSON request
--     putStrLn $ "The status code was: " ++
--                show (getResponseStatusCode response)
--     print $ getResponseHeader "Content-Type" response
--     L8.putStrLn $ getResponseBody response

data MdeloResp = ModelResp {
    id :: String,
    object :: String,
    created :: Int,
    model :: String,
    model_version :: Maybe String
} deriving (Show, Eq)

