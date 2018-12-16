module Lib.MeterReading where

import Data.Text.Encoding (decodeUtf8)
import Data.Time.Zones.All (TZLabel( Europe__Warsaw ), toTZName)
import Database.Esqueleto

import Models
import Sql


toLocalTime = timezone $ val $ decodeUtf8 $ toTZName Europe__Warsaw
dateDiff mr mr2 = (localTs mr) -. (localTs mr2)
addInterval' t interval = addInterval t (val interval)

localTs = toLocalTime . (^. MeterReadingTimestamp)
orderReadings mr = orderBy [desc (mr ^. MeterReadingTimestamp)]

usersMeter m user = m ^. MeterUser ==. val user
joinMeter m mr = mr ^. MeterReadingMeter ==. m ^. MeterId
joinUsersMeters m mr user = joinMeter m mr &&. usersMeter m user

sameMeter mr mr2 = (mr ^. MeterReadingMeter) ==. (mr2 ^. MeterReadingMeter)
joinForStats m mr mr2 = joinMeter m mr &&. sameMeter mr mr2
filterType m measureType = m ^. MeterType ==. val measureType
valDiff a b = (a ^. MeterReadingValue) -. (b ^. MeterReadingValue)
idIn mr = ((mr ^. MeterReadingId) `in_`) . subList_select . from

