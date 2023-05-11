{-# LANGUAGE OverloadedStrings #-}
module Main where

import OpenAI.Chat.API(chatCompletions)
import OpenAI.Chat.Data(respMessageContent, choiceMessages, chatCompletionChoices, exampleJson, ChatCompletion(chatCompletionId))
import System.IO
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 as BLU
      
main :: IO ()
main = do
  -- res <- chatCompletions
  -- case res of
  --   Nothing -> putStrLn "Nothing"
  --   Just x -> do
  --     let messages = choiceMessages $ head $ chatCompletionChoices x
  --     mapM_ (putStrLn . respMessageContent ) messages
  
  handle <- openFile "demo.json" ReadMode
  contents <- hGetContents handle
  print contents
  let decodedData = Data.Aeson.decode $ BLU.fromString contents :: Maybe ChatCompletion
  case decodedData of
    Nothing -> putStrLn "Nothing"
    Just x -> print $ chatCompletionId x