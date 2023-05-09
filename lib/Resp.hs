module Resp where

import Data.Aeson

data ErrorResp = ErrorResp {
    message :: String,
    type :: String,
    param :: Int,
    code :: String
} deriving (Show, Eq)