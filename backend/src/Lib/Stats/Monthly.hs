{-# LANGUAGE FlexibleContexts #-}
module Lib.Stats.Monthly where

import Database.Esqueleto

import Lib.Stats.MonthlyFilters
import Lib.Stats.TimePeriod
import Lib.MeterReading
import Sql


getMonthlyStats measureType =
  select $ from $ \(m, mr, mr2) -> do
    where_ (filterType m measureType)
    where_ (monthBoundaryValues m mr mr2)
    return $ monthlyResult mr mr2

getUserMonthlyStats userId measureType =
  select $ from $ \(m, mr, mr2) -> do
    where_ (usersMeter m userId)
    where_ (filterType m measureType)
    where_ (monthBoundaryValues m mr mr2)
    return $ monthlyResult mr mr2

sumForMonth mr mr2 targetMonth = sumUnder isTargetMonth mr mr2
  where
    isTargetMonth = getMonth mr ==. val targetMonth
    getMonth = month . monthOfMeasure

monthlyResult mr mr2 = (s 1, s 2, s 3, s 4, s 5, s 6,
                        s 7, s 8, s 9, s 10, s 11, s 12)
  where
    s = sumForMonth mr mr2
