{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module OpenAI.Models.Data where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics


data ListResp = ListResp
  {   lObject :: String,
      lData :: [Model]
  } deriving (Show)

instance FromJSON ListResp where
  parseJSON = withObject "ListResp" $ \o ->
    ListResp
      <$> o .: "object"
      <*> o .: "data"

data Model = Model
  { mId :: Text,
    mObject :: Text,
    mCreated :: Integer,
    mOwnedBy :: Text,
    mPermission :: [ModelPermission],
    mRoot :: Text,
    mParent :: Maybe Text
  }
  deriving (Show)
    
instance FromJSON Model where
  parseJSON = withObject "Model" $ \o ->
    Model
      <$> o .: "id"
      <*> o .: "object"
      <*> o .: "created"
      <*> o .: "owned_by"
      <*> o .: "permission"
      <*> o .: "root"
      <*> o .:? "parent"
      

data ModelPermission = ModelPermission
  { 
    mpId :: Text,
    mpObject :: Text,
    mpCreated :: Integer,
    mpAllowCreateEngine :: Bool,
    mpAllowSampling :: Bool,
    mpAllowLogprobs :: Bool,
    mpAllowSearchIndices :: Bool,
    mpAllowView :: Bool,
    mpAllowFineTuning :: Bool,
    mpOrganization :: Text,
    mpGroup :: Maybe Text,
    mpIsBlocking :: Bool
  }
  deriving (Show)

instance FromJSON ModelPermission where
  parseJSON = withObject "ModelPermission" $ \o ->
    ModelPermission
      <$> o .: "id"
      <*> o .: "object"
      <*> o .: "created"
      <*> o .: "allow_create_engine"
      <*> o .: "allow_sampling"
      <*> o .: "allow_logprobs"
      <*> o .: "allow_search_indices"
      <*> o .: "allow_view"
      <*> o .: "allow_fine_tuning"
      <*> o .: "organization"
      <*> o .:? "group"
      <*> o .: "is_blocking"

