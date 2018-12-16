{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Lib.Stats.Daily where

import Database.Esqueleto
import Database.PostgreSQL.PQTypes (ihours, idays, Interval(..))

import Lib.MeterReading
import Lib.Stats.TimePeriod
import Models
import Sql


getDailyStats userId measureType =
  select $ from $ \(m, mr, mr2) -> do
    where_ (joinUsersMeters m mr userId)
    where_ (filterType m measureType)
    where_ (isInBoundary mr)
    where_ (findNextDayBoundaryValue mr mr2)
    return $ dayBoundaryValuesResult mr mr2
  where
    isInBoundary mr = (toHour mr >. val since) ||. (toHour mr <. val to)
    toHour = hour . localTs
    since = 24 - untilMidnight
    to = afterMidnight

untilMidnight = 4
afterMidnight = 3
toHours = ihours . fromIntegral

getDOW = dayOfWeek . (`addInterval'` toHours (-afterMidnight)) . localTs

dayBoundaryValuesResult mr mr2 = (s 0, s 1, s 2, s 3, s 4, s 5, s 6)
  where
    s day = sumUnder (getDOW mr2 ==. val day) mr mr2

dayOfMeasure = (dateTrunc $ val "day") . normalizeDay
normalizeDay mr = localTs mr `addInterval'` toHours untilMidnight

returnLastInDay mr3 = do
  orderReadings mr3
  limit 1
  return $ mr3 ^. MeterReadingId

isLastInDay mr = mr `idIn` (
  \mr3 -> do
    where_ (sameMeter mr mr3)
    returnLastInDay mr3
  )

findNextDayBoundaryValue mr =
  (`idIn` (\mr3 -> do
        where_ (sameMeter mr mr3)
        where_ (isInNextDayBoundary mr mr3)
        orderReadings mr3
        limit 1
        return $ mr3 ^. MeterReadingId
        )
  )
  where
    isInNextDayBoundary mr mr2 = (localTs mr2) >. lowerBoundary mr
      &&. (localTs mr2) <. upperBoundary mr
    lowerBoundary mr = nextDay mr `addInterval'` toHours (-untilMidnight)
    upperBoundary mr = nextDay mr `addInterval'` toHours afterMidnight
    nextDay = (`addInterval` (val $ idays 1)) . dayOfMeasure
