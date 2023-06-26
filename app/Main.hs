{-# LANGUAGE OverloadedStrings #-}
module Main where

import OpenAI.Models.API
import System.IO
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 as BLU
import qualified Codec.Binary.UTF8.Generic as Text
      
main :: IO ()
main = do
  res <- listModels
  case res of
    Nothing -> putStrLn "Nothing"
    Just x -> print x 
      -- mapM_ (print . respMessageContent . choiceMessage) (chatCompletionChoices x)
