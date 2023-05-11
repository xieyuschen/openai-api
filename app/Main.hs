{-# LANGUAGE OverloadedStrings #-}
module Main where

import OpenAI.Chat.API(chatCompletions)
import OpenAI.Chat.Data(respMessageContent, choiceMessage, chatCompletionChoices, exampleJson, ChatCompletion(chatCompletionId))
import System.IO
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 as BLU
import qualified Codec.Binary.UTF8.Generic as Text
      
main :: IO ()
main = do
  res <- chatCompletions
  case res of
    Nothing -> putStrLn "Nothing"
    Just x -> do
      mapM_ (print . respMessageContent . choiceMessage) (chatCompletionChoices x)
