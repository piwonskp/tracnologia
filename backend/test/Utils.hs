{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Utils where

import Data.Time.Clock
import Data.Time.Calendar
import Database.Persist.Sql (insert, toSqlKey)
import Crypto.Scrypt

import Auth.User
import Models
import Db


baseDate = fromGregorian 2018 1 1
toTime = (flip UTCTime) (secondsToDiffTime 0)

toMeter meter = toSqlKey meter :: MeterId

generateReadings baseDate dailyUsage baseVal meter = [
  MeterReading (val x) (date x') (toMeter meter)
  | x <- [1..], let x'= fromIntegral x
  ]
  where
    val x = baseVal + dailyUsage * x
    date x' = toTime $ addDays x' baseDate

absoluteUsage _ [] = []
absoluteUsage acc (x:xs) = acc' : absoluteUsage acc' xs
  where
    acc' = acc + x

monthlyBaseUsage = 4200
rangeSize = 300
trigonometricUsage fun = [
  monthlyBaseUsage + (floor $ rangeSize * (fun $ x * pi / 12))
  | x <- [0..]
  ]

maxSummer = trigonometricUsage (abs . sin)
maxWinter = trigonometricUsage ((1 +) . negate . abs . sin)

sampleReadings usage baseVal meter =
  zipWith mk [0..] $ absoluteUsage baseVal usage
  where
    mk i u = MeterReading u (date i) (toMeter meter)
    date x = toTime $
      addGregorianDurationClip (CalendarDiffDays x 0) baseDate
sampleHotWater = sampleReadings maxWinter
sampleColdWater = sampleReadings maxSummer


generateDec2018 = generateReadings $ fromGregorian 2018 12 1
hotWaterExample meter = do
  runSql $ mapM insert $ take 12 $ sampleHotWater 0 meter
  runSql $ mapM insert $ take 31 $ generateDec2018 145 51718 meter
coldWaterExample meter = do
  runSql $ mapM insert $ take 12 $ sampleColdWater 0 meter
  runSql $ mapM insert $ take 31 $ generateDec2018 135 52672 meter

insertList list = runSql $ mapM insert list

addAdmin email name pass = (runSql . insert) =<< admin'
  where
    admin' = admin email name <$> (encrypt $ Pass pass)
