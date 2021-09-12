#!/usr/bin/env stack
{- stack
  script
  --resolver lts-14.14
  --package turtle
  --package morpheus-graphql-cli
  --package morpheus-graphql
  --package aeson
  --package prettyprinter
-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Turtle

main = Turtle.echo "abcd"
