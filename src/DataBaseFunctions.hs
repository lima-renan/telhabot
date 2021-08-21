{-# LANGUAGE OverloadedStrings     #-}
module DataBaseFunctions where

import           Schema

import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              filterLogger, runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.Text                   (pack)
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist            (Entity (..), delete, get, insert)
import           Database.Persist.Postgresql
import           System.Environment          (getEnv)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

getUserById :: Int -> IO (Maybe User)
getUserById uid = do
  let envStr = "host=localhost port=5432 user=star dbname=habot password=wader"
  let connStr = encodeUtf8 (pack envStr)
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connStr $ runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Maybe User)
    action = get $ UserKey (fromIntegral uid)

createUser :: User -> IO (Key User)
createUser user = do
  let envStr = "host=localhost port=5432 user=star dbname=habot password=wader"
  let connStr = encodeUtf8 (pack envStr)
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connStr $ runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Key User)
    action = insert user

insertMsg :: Message -> IO (Key Message)
insertMsg msg = do
  let envStr = "host=localhost port=5432 user=star dbname=habot password=wader"
  let connStr = encodeUtf8 (pack envStr)
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connStr $ runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Key Message)
    action = insert msg