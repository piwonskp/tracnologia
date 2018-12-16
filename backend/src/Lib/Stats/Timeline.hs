{-# LANGUAGE FlexibleContexts, DataKinds, TypeInType,
PartialTypeSignatures, ScopedTypeVariables, GADTs #-}
module Lib.Stats.Timeline where

import Database.Esqueleto

import Lib.Stats.MonthlyFilters
import Lib.MeterReading
import Models


returnTimeline m mr mr2 = do
  groupBy (m ^. MeterType, monthOfMeasure mr)
  return (m ^. MeterType, (monthOfMeasure mr, sum_ $ mr2 `valDiff` mr))

timelineStatsQuery (m, mr, mr2) = do
    where_ (monthBoundaryValues m mr mr2)
    returnTimeline m mr mr2

getUserTimelineStats userId =
  select $ from $ \(m, mr, mr2) -> do
    where_ (usersMeter m userId)
    where_ (monthBoundaryValues m mr mr2)
    returnTimeline m mr mr2

