{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataSource.ContentItems (ContentItemsRequest(..), State(..)) where

import Config (Config (..))
import ServerModel.ContentItem (ContentItem(..))

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Aeson ( FromJSON(parseJSON), Result (..) )
import Network.HTTP.Req as Req
    ( (/:),
      defaultHttpConfig,
      https,
      jsonResponse,
      oAuth2Bearer,
      req,
      runReq,
      GET(..),
      NoReqBody(NoReqBody) )
import qualified Network.HTTP.Req as Req
import Data.Text (Text)
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
import Control.Monad (unless)
import GHC.Exception.Type (Exception)
import qualified Data.List as List
import qualified Control.Applicative as Applicative
import Exceptions (FetchFailedException(FetchFailedException))
import qualified Network.HTTP.Client as HttpC
import qualified Network.HTTP.Types as HttpT
import qualified Data.ByteString as BString
import Data.Functor (($>))
import qualified Debug.Trace as Debug
import qualified Control.Exception as Ex
import Data.Monoid (Any)
import Control.Exception (SomeException)

newtype ContentItemsResponse = ContentItemsResponse { item :: ContentItem } deriving (Generic)


loadItem :: Config -> String -> IO (Result ContentItem)
loadItem Config { draftUrl, authToken, projectId } itemId = runReq defaultHttpConfig $ do
  response <-
    req
      GET
      (https (Text.pack draftUrl) /: "api" /: "project" /: Text.pack projectId /: "item" /: Text.pack itemId)
      Req.NoReqBody
      jsonResponse
      (Req.oAuth2Bearer $ fromString authToken)
  pure . fmap item . JSON.fromJSON . Req.responseBody $ response


data ContentItemsRequest a where
  GetById :: String -> ContentItemsRequest (Either String ContentItem)
  deriving (Typeable)


instance DataSource Config ContentItemsRequest where
  fetch _state _flags config = SyncFetch $ \blockedFetches -> do

    putStrLn "-----------Fetch of items started-----------------"
    let getByIdRequestVars = [(typeId, var) | BlockedFetch (GetById typeId) var <- blockedFetches] :: [(String, ResultVar (Either String ContentItem))]

    unless (null getByIdRequestVars) $ do
      loadedItem <- mapM (Ex.try . loadItem config . fst) getByIdRequestVars

      let maybeLoadedTypes = map flatEither loadedItem
      let varsWithValues = zip maybeLoadedTypes . map snd $ getByIdRequestVars

      mapM_ (uncurry $ flip putSuccess) varsWithValues

    putStrLn "-----------------Fetch of items ended-------------------"

    where
      mapFirst :: (a -> c) -> (a, b) -> (c, b)
      mapFirst mapper (toBeMapped, toBeIgnored) = (mapper toBeMapped, toBeIgnored)

      flatEither :: Either SomeException (Result a) -> Either String a
      flatEither (Left e) = Left (show e)
      flatEither (Right (Success x)) = Right x
      flatEither (Right (Error x)) = Left x


instance JSON.FromJSON ContentItemsResponse where

deriving instance Eq (ContentItemsRequest a)
deriving instance Show (ContentItemsRequest a)
instance Hashable (ContentItemsRequest a) where
  hashWithSalt salt (GetById i) = hashWithSalt salt (0 :: Int, i)

instance ShowP ContentItemsRequest where showp = show

instance StateKey ContentItemsRequest where
  data State ContentItemsRequest = ItemsState {}

instance DataSourceName ContentItemsRequest where
  dataSourceName _ = "ContentItemsDataSource"
