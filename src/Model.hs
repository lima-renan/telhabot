{-# LANGUAGE DataKinds                  #-}
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

module Model where

import Data.Text
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

    User
        userId Int
        username Text Maybe
        created UTCTime default=now()
        Primary userId
        deriving Show

    Message
        messageId Int
        userId UserId
        text Text
        sent UTCTime default=now()
        UniqueMsgUser messageId userId
        deriving Show

    Countryref
        country Text
        countryNamePt Text
        initials Text
        nationality Text
        Primary country
        deriving Show


    Countrydata
        country CountryrefId
        cases Int
        confirmed Int
        deaths Int
        recovered Int
        updatedAt UTCTime
        deriving Show Eq

    Statedata
        uid Int
        uf Text
        state Text
        cases Int
        deaths Int
        suspects Int
        refuses Int
        updatedAt UTCTime
        Primary uid
        deriving Show Eq

|]

doMigration :: SqlPersistT IO ()
doMigration = runMigration migrateAll

{- data State = State {
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
 , sigla          :: T.Text -}