{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures, Rank2Types, FlexibleContexts #-}
module Handlers.MeterReading where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Esqueleto
import Servant

import API
import Auth.Auth
import Db
import Models
import Lib.MeterReading


sameType m m2 = (m ^. MeterType) ==. (m2 ^. MeterType)

isType requested (_, readingType) = unValue readingType == requested
mkRecentMeasures readings = RecentMeasures coldWater hotWater electrical
  where
    filterType lst requestedType = map fst $ filter (isType requestedType) lst
    get = filterType readings
    coldWater = get ColdWater
    hotWater = get HotWater
    electrical = get Electrical

getRecentReadings userId =
  select $ from $ \(m, mr) -> do
    where_ $ (joinUsersMeters m mr userId)
    where_  ((mr ^. MeterReadingId) `in_` (
       subList_select $ from $ \(m2, mr2) -> do
        where_ (joinUsersMeters m2 mr2 userId)
        where_ (sameType m m2)
        orderReadings mr2
        limit 5
        return $ mr2 ^. MeterReadingId
                                    )
            )
    orderReadings mr
    return (mr, m ^. MeterType)

recentReadings :: Token -> Handler RecentMeasures
recentReadings userId = do
  readings <- liftIO $ runSql $ getRecentReadings userId
  return $ mkRecentMeasures readings

getUserReadings m mr user measureType = do
  where_ $ (joinUsersMeters m mr user) &&.
    (m ^. MeterType ==. val measureType)
  orderReadings mr


listReadings :: Token -> MeasureType -> Handler [Entity MeterReading]
listReadings userId measureType =
  liftIO $ runSql $ select $ from $ \(m, mr) -> do
    getUserReadings m mr userId measureType
    return mr

-- Exists unsupported in insert queries? Equivalent to:
-- INSERT INTO
insertMeterReadingQuery userId (MeterReading value timestamp meter) =
  insertSelectCount $ from $ \m -> do
    where_ $ (m ^. MeterId ==. val meter) &&.
      (m ^. MeterUser ==. val userId)
    return $ MeterReading <# val value <&> val timestamp <&> (m ^. MeterId)

addMeterReading :: Key User -> MeterReading -> Handler MeterReading
addMeterReading userId reading = do
  inserted <- liftIO $ insertMeterReading
  case inserted of
    False -> throwError err422
    True -> return reading
  where
    insertMeterReading = liftM isInserted $ runSql $ insertMeterReadingQuery userId reading
    isInserted 0 = False
    isInserted 1 = True
