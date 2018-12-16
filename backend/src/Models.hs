{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Models where

import Data.Aeson
import Data.Text
import Data.Time

import Database.Persist
import Database.Persist.TH

import Db

import Servant.Auth.Server
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  isActive Bool
  isAdmin Bool
  email Text
  name Text
  password Text
  UniqueEmail email
  deriving Eq Read Show
Meter json
  name Text
  type MeasureType
  user UserId
  deriving Generic Eq Read Show
MeterReading json
  value Int
  timestamp UTCTime
  meter MeterId
  deriving Eq Read Show
|]

instance ToJSON User where
  toJSON User{..} = object [
    "email" .= userEmail,
    "name" .= userName,
    "isAdmin" .= userIsAdmin
                    ]

newUser = User True
user = newUser False
admin = newUser True
