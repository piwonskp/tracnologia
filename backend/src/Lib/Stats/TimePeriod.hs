module Lib.Stats.TimePeriod where

import Database.Esqueleto

import Lib.MeterReading


sumUnder predicate mr mr2 =
  sum_ $ case_ [when_ predicate then_ (mr2 `valDiff` mr)] (else_ $ val 0)
