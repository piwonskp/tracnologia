{-# LANGUAGE OverloadedStrings #-}
module Sql where

import Data.Text (Text)
import Data.Time.Clock
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlExtractSubField, unsafeSqlBinOp,
                                       unsafeSqlFunction,
                                       SqlExpr( ERaw ), NeedParens( Never ),
                                       )
import Database.Persist.Sql (toPersistValue)
import Data.ByteString.Char8 as C
import Database.PostgreSQL.PQTypes (Interval)


instance PersistField Interval where
  toPersistValue = PersistDbSpecific . C.pack . show

epoch :: SqlExpr (Value a) -> SqlExpr (Value Int)
epoch = unsafeSqlExtractSubField "epoch"
hour :: SqlExpr (Value UTCTime) -> SqlExpr (Value Int)
hour = unsafeSqlExtractSubField "hour"
day :: SqlExpr (Value UTCTime) -> SqlExpr (Value Int)
day = unsafeSqlExtractSubField "day"
month :: SqlExpr (Value UTCTime) -> SqlExpr (Value Int)
month = unsafeSqlExtractSubField "month"
dayOfWeek :: SqlExpr (Value UTCTime) -> SqlExpr (Value Int)
dayOfWeek = unsafeSqlExtractSubField "dow"

abs_ :: SqlExpr (Value Int) -> SqlExpr (Value Int)
abs_ = unsafeSqlFunction "abs"
dateTrunc :: SqlExpr (Value Text) -> SqlExpr (Value UTCTime) -> SqlExpr (Value UTCTime)
dateTrunc = curry $ unsafeSqlFunction "date_trunc"
timezone :: SqlExpr (Value Text) -> SqlExpr (Value UTCTime) -> SqlExpr (Value UTCTime)
timezone tz datetime = unsafeSqlFunction "timezone" (tz, datetime)

addInterval :: SqlExpr (Value UTCTime) -> SqlExpr (Value a) -> SqlExpr (Value UTCTime)
addInterval = unsafeSqlBinOp " + "

