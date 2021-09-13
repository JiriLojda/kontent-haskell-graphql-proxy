{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerModel.ContentItem where

import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data ContentItem =
    ContentItem
    { id :: Text
    , name :: Text
    , codeName :: Text
    , archived :: Bool
    }
    deriving (Generic, Show)

instance FromJSON ContentItem where
