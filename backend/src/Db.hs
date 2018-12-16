{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Db where

import GHC.Generics
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.Text (unpack)
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Sql
import System.Environment (getEnv)
import Text.Casing
import Web.HttpApiData

textToMeasureType text = read (pascal $ unpack text) :: MeasureType

data MeasureType = Electrical | ColdWater | HotWater
  deriving (Generic, Show, Read, Eq)
derivePersistField "MeasureType"
instance ToJSON MeasureType
instance FromJSON MeasureType
instance FromHttpApiData MeasureType where
  parseUrlPiece = return . textToMeasureType

connectionString :: IO (ConnectionString)
connectionString =  pack <$> getEnv "DATABASE_URL"

runSql :: SqlPersistT (LoggingT IO) a ->  IO a
runSql action = do
  connStr <- connectionString
  runStdoutLoggingT $ withPostgresqlConn connStr $
    \backend -> runReaderT action backend
