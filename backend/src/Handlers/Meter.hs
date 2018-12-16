module Handlers.Meter where

import Control.Monad.IO.Class
import Database.Persist.Sql
import Database.Persist.Types
import Servant

import Auth.Auth
import Db
import Models
import API


toAPIMeter (Entity id (Meter name measureType _)) =
  SavedUserMeter id name measureType
listAPIMeter meters = map toAPIMeter meters

listMeters :: Token -> Handler [SavedUserMeter]
listMeters userId = do
  metersEntity <- liftIO $ runSql $
    selectList [MeterUser ==. userId] []
  return $ listAPIMeter metersEntity

addMeter :: Token -> UserMeter -> Handler SavedUserMeter
addMeter userId (UserMeter name meterType) = do
  let meter = Meter name meterType userId
  meterId <- liftIO $ runSql $ insert meter
  return $ toAPIMeter $ Entity meterId meter
