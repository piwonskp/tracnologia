{-# LANGUAGE OverloadedStrings #-}
module Lib.Stats.MonthlyFilters where

import Database.Esqueleto
import Database.PostgreSQL.PQTypes (imonths, idays, Interval(..))

import Lib.MeterReading
import Sql
import Models


normalizeMonth mr = (localTs mr) `addInterval'` (toDays untilNextMonth)
monthOfMeasure = (dateTrunc $ val "month") . normalizeMonth

untilNextMonth = 3
afterNextMonth = 5
toDays = idays . fromIntegral

closestToDate date mr = [asc $ abs_ $ epoch $ date -. localTs mr]
returnClosestToDate date mr = do
  orderBy $ closestToDate date mr
  limit 1
  return $ mr ^. MeterReadingId

isClosestToMonthBeginning mr =
  mr `idIn` (\mr3 -> do
        where_ (sameMeter mr mr3)
        returnClosestToDate (monthOfMeasure mr) mr3
        )

findNextMonthBoundaryValue mr =
  (`idIn` (\mr3 -> do
        where_ (sameMeter mr mr3)
        where_ (isInNextMonthBoundary mr3)
        returnClosestToDate (nextMonth mr) mr3
        ))
  where
    isInNextMonthBoundary mr3 = (localTs mr3) >. lowerBoundary mr
      &&. (localTs mr3) <. upperBoundary mr
    lowerBoundary mr = nextMonth mr `addInterval'` toDays (-untilNextMonth)
    upperBoundary mr = nextMonth mr `addInterval'` toDays afterNextMonth
    nextMonth = (`addInterval` (val $ imonths 1)) . monthOfMeasure

monthBoundaryValues m mr mr2 = joinMeter m mr &&. getMonthBoundaryValues mr mr2

getMonthBoundaryValues mr mr2 =
  isMonthBoundaryValue mr &&.
  findNextMonthBoundaryValue mr mr2
  where
    isMonthBoundaryValue mr = isInBoundary mr &&. isClosestToMonthBeginning mr
    isInBoundary mr = (day $ normalizeMonth mr) <. normalizedTimeWindow
    normalizedTimeWindow = val $ untilNextMonth + afterNextMonth
