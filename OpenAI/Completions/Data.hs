{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module OpenAI.Completions.Data where
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

data ReqCompletions= ReqCompletions
  { model :: Text,
    prompt :: Text,
    max_tokens :: Int,
    temperature :: Int
  } deriving (Generic)

instance ToJSON ReqCompletions where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data Completions = Completions
  { cId :: Text
  , cObject :: Text
  , cCreated :: Int
  , cModel :: Text
  , cChoices :: [Choice]
  , cUsage :: Usage
  }  deriving Generic

instance FromJSON Completions where
  parseJSON = withObject "ChatCompletion" $ \o ->
    Completions 
      <$> o .: "id"
      <*> o .: "object"
      <*> o .: "created"
      <*> o .: "model"
      <*> o .: "choices"
      <*> o .: "usage"


data Usage = Usage
  { cPromtTokens :: Int
  , cCompletionTokens :: Int
  , cTotalTokens :: Int
  } deriving Generic

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o ->
    Usage 
      <$> o .: "prompt_tokens"
      <*> o .: "completion_tokens"
      <*> o .: "total_tokens"

data Choice = Choice
  { cText :: Text
  , cIndex :: Int
  , cLogprobs :: Maybe Int
  , cFinishReason :: Text
  } deriving Generic

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \o ->
    Choice 
      <$> o .: "text"
      <*> o .: "index"
      <*> o .:? "logprobs"
      <*> o .: "finish_reason"

