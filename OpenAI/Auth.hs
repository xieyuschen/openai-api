module Auth where

import System.Environment (getEnv)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)


getEnvWithCheck :: String -> (String -> Bool) -> ExceptT String IO String
getEnvWithCheck envName check = do
    env <- liftIO (getEnv envName)
    if check env
        then return env
        else throwE "Not find API_KEY inside environment variables"

getAPIKey :: ExceptT String IO String
getAPIKey = getEnvWithCheck "API_KEY"  (not . null)