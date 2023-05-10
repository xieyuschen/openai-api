{-# LANGUAGE OverloadedStrings #-}
module Main where

import OpenAI.Models.API ( listModels )    
import OpenAI.Models.Data ( lData, ModelPermission, Model (mId), ListResp (ListResp) )
import Data.Aeson (decode)
import System.IO
import Data.ByteString.Lazy.UTF8 (fromString)

      
main :: IO ()
main = do
  res <- listModels
  case res of
    Nothing -> putStrLn "Nothing"
    Just x -> mapM_ (print . mId) $ lData x  