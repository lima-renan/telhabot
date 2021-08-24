{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Schema where





import Control.Monad.IO.Class


import Database.Esqueleto.PostgreSQL.JSON
import Database.Persist.Sql
import Database.Persist.TH



import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Conduit 
import Network.HTTP.Simple  
import Prelude.Compat.Repl ( Show, Int, Maybe, IO, FilePath )
import System.IO
import UnliftIO (liftIO)
import UnliftIO.Concurrent



{- 
{
  "data":[
    {
      "uid":35,
      "uf":"SP",
      "state":"São Paulo",
      "cases":4164587,
      "deaths":142528,
      "suspects":5334,
      "refuses":596,
      "datetime":"2021-08-14T22:34:41.818Z"
    },
    {
      "uid":31,
      "uf":"MG",
      "state":"Minas Gerais",
      "cases":2019435,
      "deaths":51849,
      "suspects":925,
      "refuses":104,
      "datetime":"2021-08-14T22:34:41.818Z"
    },
    {
      "uid":41,
      "uf":"PR",
      "state":"Paraná",
      "cases":1414605,
      "deaths":36432,
      "suspects":400,
      "refuses":119,
      "datetime":"2021-08-14T22:34:41.818Z"
    },
  ]
}
 -}

data State = State {
    uid       :: !(Maybe Int)
  , uf        :: !(Maybe T.Text)
  , state     :: !(Maybe T.Text)
  , cases     :: !(Maybe Int)
  , deaths    :: !(Maybe Int)
  , suspects  :: !(Maybe Int)
  , refuses   :: !(Maybe Int)
  , updatedAt :: !(Maybe UTCTime)
} deriving Show

-- para um único estado
instance FromJSON State where
  parseJSON (Object v) = State
                         <$> (v .: "uid")
                         <*> (v .: "uf")
                         <*> (v .: "state")
                         <*> (v .: "cases")
                         <*> (v .: "deaths")
                         <*> (v .: "suspects")
                         <*> (v .: "refuses")
                         <*> (v .: "datetime")
  parseJSON _ = mzero

newtype States = States { 
  states :: [State] 
} deriving Show

                     
instance FromJSON States where
    parseJSON (Object v) = do
        ms <- v .: "data"
        States <$> parseJSON ms
    parseJSON _ = mzero


{- maxStates :: [State] -> [Int]
maxStates xs = -}


-- JSON dos paises - COVID - Amostra

{- 
{
  "data": [
    {
      "country": "Canada",
      "cases": 1299,
      "confirmed": 1328,
      "deaths": 19,
      "recovered": 10,
      "updated_at": "2020-03-21T23:43:02.000Z"
    },
    {
      "country": "Maldives",
      "cases": 13,
      "confirmed": 13,
      "deaths": 0,
      "recovered": 0,
      "updated_at": "2020-03-15T18:20:18.000Z"
    },
    {
      "country": "Lithuania",
      "cases": 97,
      "confirmed": 99,
      "deaths": 1,
      "recovered": 1,
      "updated_at": "2020-03-22T02:13:18.000Z"
    }
  ]
}
 -}

data Country = Country {
    country          :: !(Maybe T.Text)
  , casesCountry     :: !(Maybe Int)
  , confirmed        :: !(Maybe Int)
  , deathsCountry    :: !(Maybe Int)
  , recovered        :: !(Maybe Int)
  , updatedAtCountry :: !(Maybe UTCTime)
} deriving Show

-- para um único país
instance FromJSON Country where
  parseJSON (Object v) = Country
                         <$> (v .: "country")
                         <*> (v .: "cases")
                         <*> (v .: "confirmed")
                         <*> (v .: "deaths")
                         <*> (v .: "recovered")
                         <*> (v .: "updated_at")
  parseJSON _ = mzero

-- para vários países
newtype Countries = Countries { 
  countries :: [Country] 
} deriving Show

                     
instance FromJSON Countries where
  parseJSON (Object v) = do
      ms <- v .: "data"
      Countries <$> parseJSON ms
  parseJSON _ = mzero

{- instance ToJSON Country where
 toJSON (Country country cases confirmed deaths recovered updated_at) =
    object [ "country"  .= country
           , "cases"   .= cases
           , "confirmed"       .= confirmed
           , "deaths" .= deaths
           , "recovered" .= recovered 
           , "updated_at" .= updated_at
             ]  -}

-- JSON nome de países - amostra
{- [ { "gentilico" : "afegãne",
    "nome_pais" : "Afeganistão",
    "nome_pais_int" : "Afghanistan",
    "sigla" : "AF"
  },
  { "gentilico" : "sul-africana",
    "nome_pais" : "África do Sul",
    "nome_pais_int" : "South Africa",
    "sigla" : "ZA"
  },
  { "gentilico" : "albanesa",
    "nome_pais" : "Albânia",
    "nome_pais_int" : "Albania",
    "sigla" : "AL"
  }
] -}

data CountriesN = CountriesN{
   gentilico      :: T.Text 
 , nome_pais      :: T.Text 
 , nome_pais_int  :: T.Text 
 , sigla          :: T.Text

}deriving (Show, Generic)

instance FromJSON CountriesN

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

    User
        userId Int
        username T.Text Maybe
        created UTCTime default=now()
        Primary userId
        deriving Show

    Message
        messageId Int
        userId UserId
        text T.Text
        sent UTCTime default=now()
        UniqueMsgUser messageId userId
        deriving Show

    Countryref
        country T.Text
        countryNamePt T.Text
        initials T.Text
        nationality T.Text
        Primary country
        deriving Show


    Countrydata
        countryName T.Text
        cases Int
        confirmed Int
        deaths Int
        recovered Int
        updatedAt UTCTime
        deriving Show Eq

    Statedata
        uid Int
        uf T.Text
        state T.Text
        cases Int
        deaths Int
        suspects Int
        refuses Int
        updatedAt UTCTime
        Primary uid
        deriving Show Eq

    Income
      source T.Text
      amount Double
      when   UTCTime
      deriving Show Eq
      
    Expense
      towhom T.Text
      amount Double
      when   UTCTime
      deriving Show Eq

|]

doMigration :: MonadIO m => SqlPersistT m ()
doMigration = runMigration migrateAll

