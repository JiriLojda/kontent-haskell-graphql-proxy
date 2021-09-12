{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerModel.ContentType (ContentType(..)) where

import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data ContentType =
    ContentType
    { id :: Text
    , name :: Text
    , codeName :: Text
    , externalId :: Maybe Text
    , archived :: Bool
    }
    deriving (Generic)

instance Show ContentType where
    show ContentType { .. } =
        Text.unpack $ "Content Type: \n\t" <>
        Text.intercalate "\n\t"
            ["id: " <> id
            , "name: " <> name
            , "codeName: " <> codeName
            , "externalId: " <> Maybe.fromMaybe "" externalId
            , "is archived: " <> Text.pack (show archived)
            ] <> "\n"

instance FromJSON ContentType where
    parseJSON = JSON.genericParseJSON (JSON.defaultOptions { JSON.fieldLabelModifier = normalizeTypeProperties })

normalizeTypeProperties :: String -> String
normalizeTypeProperties "id" = "_id"
normalizeTypeProperties prop = prop
