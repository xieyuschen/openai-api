{-# LANGUAGE OverloadedStrings #-}
module Req where

import Conf ( defaultDomain, fromDomain )
import Network.HTTP.Client (Request (host, port, secure, requestHeaders), defaultRequest)

import Data.ByteString.UTF8 as BSU ( fromString )
baseRequest :: Request
baseRequest = defaultRequest
    { host = BSU.fromString $ fromDomain defaultDomain
    , port = 443
    , secure = True
    , requestHeaders = [("Content-Type", "application/json")]
    }