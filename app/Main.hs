
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Lib                         (botStartup)
import           Schema                      (doMigration)

import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           Data.Text                   (pack)
import           Data.Text.Encoding          (encodeUtf8)
import           System.Environment          (getEnv)

main :: IO ()
main = do
  let envStr = "host=localhost port=5432 user=star dbname=habot password=wader"
  let connStr = encodeUtf8 (pack envStr)
  _ <- runStdoutLoggingT . withPostgresqlPool connStr 1 . runSqlPool $ doMigration
  botStartup