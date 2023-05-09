module Auth where

import System.Environment (getEnv)

getEnvWithCheck :: String -> (String -> Bool) -> IO(Either String String)
getEnvWithCheck envName check = do
    env <- getEnv envName
    if check env
        then return $ Right env
        else return $ Left "Not find API_KEY inside environment variables"

getAPIKey :: IO(Either String String)
getAPIKey = do
    -- todo: refine this code, I don't think it's a good way.
    eitherKey <- getEnvWithCheck "API_KEY" null
    case eitherKey of 
        Left x -> return $ error x
        Right x -> return $ Right x