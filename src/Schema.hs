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
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

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

data Country = Country {
    country          :: !(Maybe T.Text)
  , casesCountry     :: !(Maybe Int)
  , confirmed        :: !(Maybe Int)
  , deathsCountry    :: !(Maybe Int)
  , recovered        :: !(Maybe Int)
  , updatedAtCountry :: !(Maybe UTCTime)
} deriving Show

data CountriesN = CountriesN{
   gentilico      :: T.Text 
 , nome_pais      :: T.Text 
 , nome_pais_int  :: T.Text 
 , sigla          :: T.Text
}deriving (Show, Generic)