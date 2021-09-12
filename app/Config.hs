{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Config (
    load,
    Config (..)
) where

import GHC.Generics (Generic)
import Data.Yaml as Yaml

data Config = Config
    {   draftUrl :: String
    ,   authToken :: String
    ,   projectId :: String
    } deriving (Generic, Show)

instance Yaml.FromJSON Config

load :: IO (Either String Config)
load = do
    content <- Yaml.decodeFileEither "./config.yaml"
    case content of
        Left err -> pure (Left $ "Failed to load config.yaml: " ++ show err)
        Right config -> pure $ Right config
