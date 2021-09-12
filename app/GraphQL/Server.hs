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

module GraphQL.Server (createGqlEndpoint) where

import DataSource.Types (ContentTypesRequest(..), State (TypesState))
import qualified ServerModel.ContentType as ServerModel
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
import Haxl.Core (StateKey)
import Data.List (foldl')

MorpDoc.importGQLDocument "schema.graphql"

rootResolver :: RootResolver MyHaxl () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {types, typeById},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    types :: ComposedResolver QUERY e MyHaxl [] ContentType
    types = lift $ fmap contentTypeFromServerModel <$> Haxl.dataFetch GetAllTypes

    typeById :: TypeByIdArgs -> ComposedResolver QUERY e MyHaxl Maybe ContentType
    typeById = lift . fmap (Just . contentTypeFromServerModel) . Haxl.dataFetch . GetById . Text.unpack . typeId

contentTypeFromServerModel :: (Applicative a) => ServerModel.ContentType -> ContentType a
contentTypeFromServerModel ServerModel.ContentType { .. } = ContentType
  { id = pure id
  , name = pure name
  , codeName = pure codeName
  , externalId = pure externalId
  , archived = pure archived
  }

app :: App () MyHaxl
app = Morp.deriveApp rootResolver

interpreter :: (MapAPI a b) => a -> MyHaxl b
interpreter = Morp.interpreter rootResolver

haxlStates =
  [ TypesState
  ]

runInterpreter :: MapAPI a b => Config -> a -> IO b
runInterpreter config input = do
  let stateStore = foldl' (flip Haxl.stateSet) Haxl.stateEmpty haxlStates
  environment <- Haxl.initEnv stateStore config
  Haxl.runHaxl environment (interpreter input)

createGqlEndpoint :: Config -> RoutePattern ->  ScottyM ()
createGqlEndpoint config route = do
  Scotty.get route $ Scotty.raw MorpServer.httpPlayground
  Scotty.post route $ Scotty.raw =<< (liftIO . runInterpreter config =<< Scotty.body)
