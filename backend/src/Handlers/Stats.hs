{-# LANGUAGE FlexibleContexts, DataKinds, TypeInType,
PartialTypeSignatures, ScopedTypeVariables, GADTs #-}
module Handlers.Stats where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens (over, each, view, _tail, _1)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Text.Lazy.Builder (fromText)
import Data.Time.Calendar.WeekDate
import Data.Time.Clock (utctDay)
import Data.Time.LocalTime
import Data.Tuple.Curry (uncurryN)
import Data.Tuple.Homogenous (Tuple12(..), untuple12)
import Data.Tuple.Select
import Data.Dynamic (dynApp, fromDynamic, toDyn)
import qualified Data.List as List
import Data.Maybe (fromJust)

import Database.Esqueleto
import Lib.Stats.MonthlyFilters

import Servant

import API
import Auth.Auth
import Db
import Models
import Lib.Stats.Timeline
import Lib.Stats.Monthly
import Lib.Stats.Daily
import Sql


maybeRationalToInt = truncate . (fromMaybe (0 ::Rational))
sumValueToInt = maybeRationalToInt . unValue

toMonthlyStats = (uncurryN MonthlyStats) . toTuple
  where
    -- Lens works for up to 10 element tuples only
    toTuple = untuple12 . fmap sumValueToInt . Tuple12 . head

userMonthlyStats :: Token -> MeasureType -> Handler MonthlyStats
userMonthlyStats user measureType = toMonthlyStats <$> fetch
  where
    fetch = liftIO $ runSql $ getUserMonthlyStats user measureType

monthlyStats :: MeasureType -> Handler MonthlyStats
monthlyStats measureType = toMonthlyStats <$> fetch
  where
    fetch = liftIO $ runSql $ getMonthlyStats measureType

dailyStats :: Token -> MeasureType -> Handler DailyStats
dailyStats user measureType = uncurryN DailyStats <$> toTuple <$> fetch
  where
    toTuple = (over each sumValueToInt) . head
    fetch = liftIO $ runSql $ getDailyStats user measureType

toTimelineStats all = TimelineStats (ft ColdWater) (ft HotWater) (ft Electrical)
  where
    ft = forType
    forType = (map $ uncurryN SingleMonthUsage) . timeToDay . dropType . filterType
    timeToDay = map ((YearMonth . utctDay) *** maybeRationalToInt)
    filterType typ = filter ((== typ) . fst) all
    dropType = map snd

unValTimeline = map $ unValue *** unValue *** unValue

userTimelineStats :: Token -> Handler TimelineStats
userTimelineStats user = toTimelineStats <$> unValTimeline <$> fetch
  where
    fetch = liftIO $ runSql $ getUserTimelineStats user

timelineStats :: Handler TimelineStats
timelineStats = toTimelineStats <$> unValTimeline <$> fetch
  where
    fetch = liftIO $ runSql $ getTimelineStats
    -- Haskell can't deduce type if function has no arguments and is in other module than handler
    getTimelineStats = (select . from) timelineStatsQuery
