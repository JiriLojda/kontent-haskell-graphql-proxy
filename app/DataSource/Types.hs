{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataSource.Types (ContentTypesRequest(..), State(..)) where

import Config (Config (..))
import ServerModel.ContentType (ContentType)
import qualified ServerModel.ContentType as ContentType

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Aeson ( FromJSON(parseJSON), Result (..) )
import Network.HTTP.Req as Req
    ( (/:),
      defaultHttpConfig,
      https,
      jsonResponse,
      oAuth2Bearer,
      req,
      responseBody,
      runReq,
      GET(..),
      NoReqBody(NoReqBody) )
import qualified Network.HTTP.Req as Req
import qualified Data.Text as Text
import qualified Data.Text.Internal.ByteStringCompat
import Data.String ( IsString(..) )
import qualified Data.Aeson.Types as JSON
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Hashable (Hashable(..))
import Haxl.Core (ShowP(..), DataSourceName (..), BlockedFetch (..), putSuccess, PerformFetch (SyncFetch), putFailure, ResultVar)
import Haxl.Core.StateStore (StateKey(..))
import Haxl.Core.DataSource (DataSource(fetch))
import Control.Monad (unless, (<=<), (>=>))
import GHC.Exception.Type (Exception)
import qualified Data.List as List
import qualified Control.Applicative as Applicative
import Exceptions (FetchFailedException(FetchFailedException))

newtype TypesResponse = TypesResponse { _data :: [ContentType] } deriving (Generic)


loadAllContentTypes :: Config -> IO (Result [ContentType])
loadAllContentTypes Config { draftUrl, authToken, projectId } = runReq defaultHttpConfig $ do
  response <-
    req
      GET
      (https (Text.pack draftUrl) /: "api" /: "project" /: Text.pack projectId /: "type")
      Req.NoReqBody
      jsonResponse
      (Req.oAuth2Bearer $ fromString authToken)
  liftIO $ putStrLn "Fetching types from the BE!!!"
  pure . fmap _data . JSON.fromJSON . responseBody $ response


data ContentTypesRequest a where
  GetAllTypes :: ContentTypesRequest [ContentType]
  GetById :: String -> ContentTypesRequest ContentType
  deriving (Typeable)


instance DataSource Config ContentTypesRequest where
  fetch _state _flags config = SyncFetch $ \blockedFetches -> do

    let getAllRequestVars = [var | BlockedFetch GetAllTypes var <- blockedFetches]
    putStrLn "-----------Fetch of types started-----------------"

    abc <- if null getAllRequestVars
      then pure Nothing
      else do
        putStrLn "Inside get all fetch"
        loadedTypes <- loadAllContentTypes config

        let mapFnc = case loadedTypes of
              Error e -> (`putFailure` FetchFailedException e)
              Success types -> (`putSuccess` types)

        mapM_ mapFnc getAllRequestVars

        pure $ resultToMaybe loadedTypes

    let getByIdRequestVars = [(typeId, var) | BlockedFetch (GetById typeId) var <- blockedFetches] :: [(String, ResultVar ContentType)]

    unless (null getByIdRequestVars) $ do
      putStrLn "Inside by id fetch"
      loadedTypes2 <- maybe (loadAllContentTypes config) (pure . Success) abc

      let varsWithValues = map (mapFirst (maybe (Error typeNotFound) Success . flip findTypeById loadedTypes2)) getByIdRequestVars

      mapM_ putResultIntoVar varsWithValues

    putStrLn "-----------------Fetch of types ended-------------------"

    where
      resultToMaybe :: Result a -> Maybe a
      resultToMaybe (Error _) = Nothing
      resultToMaybe (Success res) = Just res

      findTypeById :: String -> Result [ContentType] -> Maybe ContentType
      findTypeById typeId = resultToMaybe >=> List.find ((== typeId) . Text.unpack . ContentType.id)

      mapFirst :: (a -> c) -> (a, b) -> (c, b)
      mapFirst mapper (toBeMapped, toBeIgnored) = (mapper toBeMapped, toBeIgnored)

      putResultIntoVar :: (Result ContentType, ResultVar ContentType) -> IO ()
      putResultIntoVar (Error e, var) = putFailure var $ FetchFailedException e
      putResultIntoVar (Success contentType, var) = putSuccess var contentType


instance JSON.FromJSON TypesResponse where
  parseJSON = JSON.genericParseJSON (JSON.defaultOptions { JSON.fieldLabelModifier = dataPropertyReplacer })

dataPropertyReplacer :: String -> String
dataPropertyReplacer "_data" = "data"
dataPropertyReplacer prop = prop

deriving instance Eq (ContentTypesRequest a)
deriving instance Show (ContentTypesRequest a)
instance Hashable (ContentTypesRequest a) where
  hashWithSalt salt GetAllTypes = hashWithSalt salt (0 :: Int)
  hashWithSalt salt (GetById i) = hashWithSalt salt (1 :: Int, i)

instance ShowP ContentTypesRequest where showp = show

instance StateKey ContentTypesRequest where
  data State ContentTypesRequest = TypesState {}

instance DataSourceName ContentTypesRequest where
  dataSourceName _ = "ContentTypesDataSource"

typeNotFound :: String
typeNotFound = "Type with the given id not found."
