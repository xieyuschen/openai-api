{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module OpenAI.Chat.Data where
import Data.Aeson
    ( ToJSON(toJSON, toEncoding),
      defaultOptions,
      genericToEncoding,
      genericToJSON, FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, withObject, (.:), (.:?) )
import Data.Aeson.Types (defaultOptions)
import Data.Aeson.Encode.Pretty (encodePretty)
import OpenAI.Models.Data
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics

data Message = Message
  { role :: Text,
    content :: Text
  }
  deriving (Generic)

data ChatRequest = ChatRequest
  { model :: Text,
    messages :: [Message],
    temperature :: Int,
    top_p :: Int,
    n :: Int,
    stream :: Bool,
    max_tokens :: Int,
    presence_penalty :: Int,
    frequency_penalty :: Int
  }
  deriving (Generic)

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ChatRequest where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

exampleJson :: ChatRequest
exampleJson =
  ChatRequest
    { model = "gpt-3.5-turbo",
      messages =
        [ Message
            { role = "user",
              content = "Who won the world series in 2020?"
            },
          Message
            { role = "assistant",
              content = "The Los Angeles Dodgers won the World Series in 2020."
            },
          Message
            { role = "user",
              content = "Where was it played?"
            }
        ],
      temperature = 1,
      top_p = 1,
      n = 1,
      stream = False,
      max_tokens = 250,
      presence_penalty = 0,
      frequency_penalty = 0
    }


data ChatCompletion = ChatCompletion
  { chatCompletionId :: Text
  , chatCompletionObject :: Text
  , chatCompletionCreated :: Int
  , chatCompletionModel :: Text
  , chatCompletionUsage :: Usage
  , chatCompletionChoices :: [Choice]
  }  deriving Generic

instance FromJSON ChatCompletion where
  parseJSON = withObject "ChatCompletion" $ \o ->
    ChatCompletion 
      <$> o .: "id"
      <*> o .: "object"
      <*> o .: "created"
      <*> o .: "model"
      <*> o .: "usage"
      <*> o .: "choices"

data Usage = Usage
  { usagePromptTokens :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens :: Int
  } deriving Generic

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o ->
    Usage 
      <$> o .: "prompt_tokens"
      <*> o .: "completion_tokens"
      <*> o .: "total_tokens"

data Choice = Choice
  { choiceMessage :: RespMessage
  , choiceFinishReason :: Text
  , choiceIndex :: Int
  } deriving Generic

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \o ->
    Choice 
      <$> o .: "message"
      <*> o .: "finish_reason"
      <*> o .: "index"

data RespMessage = RespMessage 
  { respMessageRole :: Text
  , respMessageContent :: Text
  } deriving Generic

instance FromJSON RespMessage where
  parseJSON = withObject "RespMessage" $ \o ->
    RespMessage 
      <$> o .: "role"
      <*> o .: "content"

