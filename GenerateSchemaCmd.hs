#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.6
  --package turtle
  --package morpheus-graphql-cli
  --package morpheus-graphql
  --extra-dep morpheus-graphql-app-0.17.0
  --extra-dep morpheus-graphql-core-0.17.0
  --extra-dep optparse-applicative-0.14.3.0
-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Turtle

main = Turtle.echo "abcd"
