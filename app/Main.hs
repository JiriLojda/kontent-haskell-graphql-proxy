{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import DataSource.Types as Types
import Data.Aeson (Result(..))
import ServerModel.ContentType (ContentType)
import Haxl.Env (MyHaxl)
import Haxl.Prelude (dataFetch)
import qualified Haxl.Core as Haxl
import Haxl.Core.Fetch (dataFetch)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Haxl.Core (dataFetch)
import qualified GraphQL.Server as Gql
import qualified Web.Scotty as Scotty
import System.IO (BufferMode (LineBuffering))
import qualified System.IO as SIO

main :: IO ()
main = do
    SIO.hSetBuffering SIO.stdout LineBuffering
    putStrLn "Loading config..."
    eitherConfig <- Config.load
    case eitherConfig of
      Left s -> putStrLn s
      Right config -> do
        putStrLn $ "Loaded: " ++ show config
        Scotty.scotty 3000 $ Gql.createGqlEndpoint config "/api"
        -- putStrLn "Trying to send something"

        -- let stateStore = Haxl.stateSet TypesState{} Haxl.stateEmpty
        -- env0 <- Haxl.initEnv stateStore config

        -- types <- Haxl.runHaxl env0 $ do
        --   myResult <- loadTypesTest
        --   dataFetch GetAllTypes
        --   loadOneTypeTest "e91088e6-24fa-45f2-80e4-183e05507318"
        --   return myResult

        -- putStrLn ("Loaded types using Haxl: " ++ show types)
        -- putStrLn "Loading cached type: "

        -- hereYouAre <- Haxl.runHaxl env0 $ loadOneTypeTest "e91088e6-24fa-45f2-80e4-183e05507318"

        -- putStrLn ("Loaded most probably not cached: " ++ show hereYouAre)


-- typesToLoad :: [String]
-- typesToLoad = ["17fe0013-23c1-4d28-9312-6b9589d21c31", "6d499588-b9ac-4c7d-885c-d18570dfae9c", "0a4285bf-1014-424d-acad-914647bc2d2a"]

-- loadTypesTest :: MyHaxl [ContentType]
-- loadTypesTest = do
--   res <- mapM (dataFetch . GetById) typesToLoad
--   dataFetch GetAllTypes
--   dataFetch $ GetById "e91088e6-24fa-45f2-80e4-183e05507318"
--   pure res

-- loadOneTypeTest :: String -> MyHaxl ContentType
-- loadOneTypeTest = dataFetch . GetById
