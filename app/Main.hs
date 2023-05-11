{-# LANGUAGE OverloadedStrings #-}
module Main where

import OpenAI.Models.API ( listModels , retrieveModel)    
import OpenAI.Models.Data ( lData, mId, ModelPermission, Model (mId), ListResp (ListResp) )
import Data.Aeson (decode)
import System.IO
import Data.ByteString.Lazy.UTF8 (fromString)

      
main :: IO ()
main = do
  res <- retrieveModel "text-ada:001"  
  case res of
    Nothing -> putStrLn "Nothing"
    Just x -> print $ mId x