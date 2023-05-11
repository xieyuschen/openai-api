module Auth where

import System.Environment (getEnv)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
getEnvWithCheck :: String -> (String -> Bool) -> IO (Either String String)
getEnvWithCheck envName check = do
    env <- getEnv envName
    if check env
        then return $ Right env
        else return $ Left "Not find API_KEY inside environment variables"

getAPIKey :: IO (Either String String)
getAPIKey = do
    -- todo: refine this code, I don't think it's a good way.
    eitherKey <- getEnvWithCheck "API_KEY" $ not . null
    case eitherKey of 
        Left x -> return $ error x
        Right x -> return $ Right x





getEnvWithCheck' :: String -> (String -> Bool) -> ExceptT String IO String
getEnvWithCheck' envName check = do
    env <- liftIO (getEnv envName)
    if check env
        then return env
        else throwE "Not find API_KEY inside environment variables"

getAPIKey' :: ExceptT String IO String
getAPIKey' = getEnvWithCheck' "API_KEY"  (not . null)