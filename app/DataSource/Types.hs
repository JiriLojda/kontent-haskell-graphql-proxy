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
import qualified Control.Exception as Ex
import Control.Exception (SomeException)

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
  pure . fmap _data . JSON.fromJSON . responseBody $ response


data ContentTypesRequest a where
  GetAllTypes :: ContentTypesRequest (Either String [ContentType])
  GetById :: String -> ContentTypesRequest (Either String ContentType)
  deriving (Typeable)


instance DataSource Config ContentTypesRequest where
  fetch _state _flags config = SyncFetch $ \blockedFetches -> do

    let getAllRequestVars = [var | BlockedFetch GetAllTypes var <- blockedFetches]
    putStrLn "-----------Fetch of types started-----------------"

    abc <- if null getAllRequestVars
      then pure . Left $ ("No need to fetch all" :: String)
      else do
        putStrLn "Inside get all fetch"
        loadedTypes <- Ex.try . loadAllContentTypes $ config :: IO (Either SomeException (Result [ContentType]))

        mapM_ (`putSuccess` flatEither loadedTypes) getAllRequestVars

        pure $ flatEither loadedTypes

    let getByIdRequestVars = [(typeId, var) | BlockedFetch (GetById typeId) var <- blockedFetches] :: [(String, ResultVar (Either String ContentType))]

    unless (null getByIdRequestVars) $ do
      putStrLn "Inside by id fetch"
      loadedTypes2 <- either (const . Ex.try $ loadAllContentTypes config) (pure . Right . Success) abc :: IO (Either SomeException (Result [ContentType]))

      let varsWithValues = case loadedTypes2 of
            Left e -> map (mapFirst . const . Left . show $ e) getByIdRequestVars
            Right (Error s) -> map (mapFirst . const . Left $ s) getByIdRequestVars
            Right (Success cts) -> map (mapFirst $ flip findTypeById cts) getByIdRequestVars

      mapM_ (uncurry $ flip putSuccess) varsWithValues

    putStrLn "-----------------Fetch of types ended-------------------"

    where
      notFoundMsg :: String -> String
      notFoundMsg typeId = "Type with id " ++ typeId ++ " doesn't exist."

      findTypeById :: String -> [ContentType] -> Either String ContentType
      findTypeById typeId = maybe (Left $ notFoundMsg typeId) Right . List.find ((== typeId) . Text.unpack . ContentType.id)

      mapFirst :: (a -> c) -> (a, b) -> (c, b)
      mapFirst mapper (toBeMapped, toBeIgnored) = (mapper toBeMapped, toBeIgnored)

      flatEither :: Either SomeException (Result a) -> Either String a
      flatEither (Left e) = Left (show e)
      flatEither (Right (Success x)) = Right x
      flatEither (Right (Error x)) = Left x


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
