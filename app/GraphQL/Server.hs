{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module GraphQL.Server (createGqlEndpoint) where

import DataSource.Types (State (TypesState))
import qualified DataSource.Types as TypesSource
import DataSource.ContentItems (State(ItemsState))
import qualified DataSource.ContentItems as ItemsSource
import qualified ServerModel.ContentType as TypeServerM
import qualified ServerModel.ContentItem as ItemServerM
import Haxl.Env (MyHaxl)

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Morpheus as Morp
import qualified Data.Morpheus.Document as MorpDoc
import Data.Morpheus.Types (RootResolver (..), Undefined (..), ResolverQ, ComposedResolver, QUERY, lift, App)
import qualified Data.Morpheus.Types as MorpTypes
import qualified Data.Morpheus.Server as MorpServer
import Data.Text (Text)
import qualified Haxl.Core as Haxl
import qualified Data.Text as Text
import Data.Morpheus.App (MapAPI)
import Web.Scotty (RoutePattern, ScottyM)
import qualified Web.Scotty as Scotty
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Config (Config)
import Haxl.Core (StateKey, StateStore)
import Data.List (foldl')
import Control.Monad ((<=<))

MorpDoc.importGQLDocument "schema.graphql"

rootResolver :: RootResolver MyHaxl () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query
      { types = typesResolver
      , typeById = typeByIdResolver
      , itemById = itemByIdResolver
      },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }


typesResolver :: ComposedResolver QUERY e MyHaxl [] ContentType
typesResolver = mapM contentTypeFromServerModel <=< lift . Haxl.dataFetch $ TypesSource.GetAllTypes

typeByIdResolver :: TypeByIdArgs -> ComposedResolver QUERY e MyHaxl Maybe ContentType
typeByIdResolver = resolveTypeId . typeId

resolveTypeId :: Text -> ComposedResolver QUERY e MyHaxl Maybe ContentType
resolveTypeId = mapM contentTypeFromServerModel <=< lift . Haxl.dataFetch . TypesSource.GetById . Text.unpack

itemByIdResolver :: ItemByIdArgs -> ComposedResolver QUERY e MyHaxl Maybe ContentItem
itemByIdResolver = mapM contentItemFromServerModel <=< lift . Haxl.dataFetch . ItemsSource.GetById . Text.unpack . itemId


contentTypeFromServerModel :: TypeServerM.ContentType -> ResolverQ e MyHaxl ContentType
contentTypeFromServerModel TypeServerM.ContentType { .. } = pure ContentType
  { id = pure id
  , name = pure name
  , codeName = pure codeName
  , externalId = pure externalId
  , archived = pure archived
  }


contentItemFromServerModel :: ItemServerM.ContentItem -> ResolverQ e MyHaxl ContentItem
contentItemFromServerModel ItemServerM.ContentItem { .. } = pure ContentItem
  { id = pure id
  , name = pure name
  , codeName = pure codeName
  , archived = pure archived
  , type' = resolveTypeId (ItemServerM._id typeRef) >>= failOnNothing "Corrupted data, can't find type of content item."
  }

failOnNothing :: (MonadFail m) => String -> Maybe a -> m a
failOnNothing _ (Just a) = pure a
failOnNothing msg Nothing = fail msg


app :: App () MyHaxl
app = Morp.deriveApp rootResolver

interpreter :: (MapAPI a b) => a -> MyHaxl b
interpreter = Morp.interpreter rootResolver

data HaxlSourceState = forall a. StateKey a => HaxlSourceState (State a)

haxlStates :: [HaxlSourceState]
haxlStates =
  [ HaxlSourceState TypesState
  , HaxlSourceState ItemsState
  ]

runInterpreter :: MapAPI a b => Config -> a -> IO b
runInterpreter config input = do
  let stateStore = foldl' addToState Haxl.stateEmpty haxlStates
  environment <- Haxl.initEnv stateStore config
  Haxl.runHaxl environment (interpreter input)
  where
    addToState :: StateStore -> HaxlSourceState -> StateStore
    addToState store (HaxlSourceState st) = Haxl.stateSet st store

createGqlEndpoint :: Config -> RoutePattern ->  ScottyM ()
createGqlEndpoint config route = do
  Scotty.get route $ Scotty.raw MorpServer.httpPlayground
  Scotty.post route $ Scotty.raw =<< (liftIO . runInterpreter config =<< Scotty.body)
