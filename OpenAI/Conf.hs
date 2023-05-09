module Conf where

-- define domain names
-- todo: check further whether this is the right way to do it.
newtype Domain = Domain String deriving (Show, Eq)
defaultDomain :: Domain
defaultDomain = Domain "api.openai.com"

fromDomain :: Domain -> String
fromDomain (Domain d) = d
