{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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



import            Control.Monad.IO.Class
import            Database.Persist.Sql
import            Database.Persist.TH
import            Database.Persist.Postgresql (ConnectionString)
import qualified  Data.Text as T
import            Data.Time (UTCTime)
import            GHC.Generics (Generic)








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

    CountriesNames
        nationality  T.Text
        countryNamePt T.Text
        country T.Text
        initials T.Text
        Primary country
        deriving Show Read Eq

    Country
        countryName T.Text
        cases Int
        confirmed Int
        deaths Int
        recovered Int
        updatedAt UTCTime
        deriving Show Read Eq

    State
        uid Int
        uf T.Text
        state T.Text
        cases Int
        deaths Int
        suspects Int
        refuses Int
        updatedAt UTCTime
        Primary uid
        deriving Show Read Eq

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

--run postgres
--open bash terminal
-- psql -U root
-- \c habot
-- \dt for list tables
-- use ; : SELECT * FROM user;