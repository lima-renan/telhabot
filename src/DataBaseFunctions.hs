{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module DataBaseFunctions where

import           Schema
import           Control.Monad.IO.Class           (liftIO, MonadIO)
import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              filterLogger, runStdoutLoggingT)
import           Control.Monad.Reader        (ReaderT, runReaderT,forM)
import           Control.Monad.IO.Unlift
import           Data.Int (Int64)
import           qualified Data.Text as T                
import           Database.Esqueleto.Legacy
import           qualified Database.Persist            (Entity (..), delete, get, insert)
import           Database.Persist.Postgresql


connStr::ConnectionString
connStr = "host=localhost port=5432 user=star dbname=habot password=wader"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

runDb :: SqlPersistT (LoggingT IO) a -> IO a
runDb query = do
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connStr $ \backend -> runReaderT query backend

getUserById :: Int -> IO (Maybe User)
getUserById uid = do
  runDb $ get $ UserKey (fromIntegral uid)

createUser :: User -> IO (Key User)
createUser user = do
  runDb $ insert user

insertMsg :: Message -> IO (Key Message)
insertMsg msg = do
  runDb $ insert msg

insertStates :: [State] -> IO ([Key State])
insertStates st = do
  runDb $ mapM insert st

insertCountriesN :: [CountryName] -> IO ([Key CountryName])
insertCountriesN ct = do
  runDb $ mapM insert ct

insertCountries :: [Country] -> IO ([Key Country])
insertCountries ct = do
  runDb $ mapM insert ct



countCountriesN :: IO [Value Int64]
countCountriesN = runDb $ select $ distinct $ from $ \(country_name :: SqlExpr (Entity CountryName)) -> return countRows



{- selectCount:: IO [(Entity CountriesNames)]
selectCount = runDb $ selectionAction
  where
    selectionAction :: SqlPersistT (LoggingT IO) [(Entity CountriesNames)]
    selectionAction = select . from $ \countries_names -> do
      return (countries_names)
 -}


--https://stackoverflow.com/questions/24020739/what-is-a-correct-way-of-doing-countid-in-esqueleto-and-yesod

{- selectCount :: ReaderT backend0 m0 [Value Int]
selectCount = select $ 
              from $ \countries_names -> do
              return (countRows) -}
  


{- getCountriesN :: MonadIO m => ReaderT SqlBackend m [Single Text]
getCountriesN = rawSql "select countries_names.country from countries_names"  -}


{- convertState :: [State] -> Statedata
convertState st = head st  -}
