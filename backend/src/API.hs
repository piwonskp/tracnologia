{-# LANGUAGE DataKinds, TypeOperators, DuplicateRecordFields,
             DeriveGeneric, OverloadedStrings, StandaloneDeriving,
             TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable,
             PartialTypeSignatures, FlexibleContexts, MultiParamTypeClasses
#-}

module API where

import GHC.Generics (Generic)
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as LB
import Data.Coerce (coerce)
import Data.Text
import Data.Text.Encoding
import Data.Time (Day, toGregorian)
import Data.Time.Format.ISO8601 (formatShow, yearMonthFormat)
import Data.Typeable (Typeable)
import Database.Persist (Entity)
import Crypto.Scrypt (Pass(..))
import Servant
import Servant.API
import Servant.Auth.Server
import Network.HTTP.Types (status401)
import Network.HTTP.Media ((//), (/:))
import Servant.Checked.Exceptions (NoThrow, Throws)
import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus))

import Auth.Auth
import Models
import Db

import Servant.Checked.Exceptions

data SavedUserMeter = SavedUserMeter {id :: MeterId, name :: Text,
                                      _type :: MeasureType}
  deriving (Generic, Show)
data UserMeter = UserMeter {name :: Text, _type :: MeasureType}
  deriving (Generic, Show)
unhaskellLabel "_type" = "type"
unhaskellLabel l = l
opts = defaultOptions {fieldLabelModifier = unhaskellLabel}

instance ToJSON SavedUserMeter where toJSON = genericToJSON opts
instance FromJSON SavedUserMeter where parseJSON = genericParseJSON opts
instance ToJSON UserMeter where toJSON = genericToJSON opts
instance FromJSON UserMeter where parseJSON = genericParseJSON opts

type MeasureAPI = "meterreading" :> "list" :> Get '[JSON] [Entity MeterReading]

data MonthlyStats = MonthlyStats {january :: Int, february :: Int,
                                  march :: Int, april :: Int,
                                  may :: Int, june :: Int,
                                  july :: Int, august :: Int,
                                  september :: Int, october :: Int,
                                  november :: Int, december :: Int}
  deriving (Generic, Show, Typeable)

data DailyStats = DailyStats {monday :: Int, tuesday :: Int,
                             wednesday :: Int, thursday :: Int,
                             friday :: Int, saturday :: Int,
                             sunday :: Int}
  deriving (Generic, Show)
instance ToJSON MonthlyStats; instance FromJSON MonthlyStats
instance ToJSON DailyStats; instance FromJSON DailyStats

newtype YearMonth = YearMonth Day deriving (Generic, Show)
instance ToJSON YearMonth where
  toJSON dt = let (y, m, _) = toGregorian $ coerce dt in
    toJSON $ formatShow yearMonthFormat (y, m)
data SingleMonthUsage = SingleMonthUsage {date :: YearMonth, usage :: Int}
  deriving (Generic, Show)
data TimelineStats = TimelineStats {coldWater :: [SingleMonthUsage],
                                    hotWater :: [SingleMonthUsage],
                                    electrical :: [SingleMonthUsage]}
  deriving (Generic, Show)
instance ToJSON SingleMonthUsage;
instance ToJSON TimelineStats;

data RecentMeasures = RecentMeasures {coldWater :: [Entity MeterReading],
                                      hotWater :: [Entity MeterReading],
                                      electrical :: [Entity MeterReading]
                                     }
  deriving (Generic, Show)
instance ToJSON RecentMeasures

data UserContext = UserContext {user :: User, meters :: [SavedUserMeter]}
  deriving (Generic, Show)
instance ToJSON UserContext

type MeterAPI = "list" :> Get '[JSON] [SavedUserMeter]
  :<|> "add" :> ReqBody '[JSON] UserMeter :> Post '[JSON] SavedUserMeter

type MeasureTypeStats =
  "daily" :> Get '[JSON] DailyStats
  :<|>
  "monthly" :> Get '[JSON] MonthlyStats
type Stats = "type" :> Capture "measureType" MeasureType :> MeasureTypeStats
  :<|> "timeline" :> Get '[JSON] TimelineStats

data ChangePassword = ChangePassword {password :: Pass, newPassword :: Pass}
   deriving (Eq, Show, Generic)
instance FromJSON ChangePassword

type Protected =
  "get-context" :> Get '[JSON] UserContext
  :<|> "meter" :> MeterAPI
  :<|> "measure-types" :> Capture "measureType" MeasureType :> MeasureAPI
  :<|> "add-meterreading" :> ReqBody '[JSON] MeterReading :> Post '[JSON] MeterReading
  :<|> "recent-readings" :> Get '[JSON] RecentMeasures

type MaybeUser = "stats" :> Stats

deriving instance Generic Pass
instance FromJSON Pass where
  parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True }
instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ return . encodeUtf8
instance ToJSON Pass
-- Unused but needed for compiler
instance ToJSON ByteString where
  toJSON _ = error "Serializing of ByteStrings is not supported"

data NewUser = NewUser {email :: Text, password :: Pass, name :: Text }
   deriving (Eq, Show, Generic)
instance FromJSON NewUser

data Login = Login { email :: Text, password :: Pass }
   deriving (Eq, Show, Generic)
instance FromJSON Login

type Unprotected =
  "login" :> ReqBody '[JSON] Login
     :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                               , Header "Set-Cookie" SetCookie]
                       UserContext)

type Admin =
  "users" :> Get '[JSON] [User]
  :<|>
  "users" :> Capture "userId" UserId :> Get '[JSON] User
  :<|> "add-user" :> ReqBody '[JSON] NewUser :> Post '[JSON] User

data Test = Test {dupa :: String}
  deriving (Generic, Show)
instance ToJSON Test

type WithUser =
  "change-password" :> ReqBody '[JSON] ChangePassword :> PostNoContent '[JSON] NoContent

data HTML
newtype RawHtml = RawHtml { unRaw :: LB.ByteString }
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML RawHtml where
    mimeRender _ =  unRaw
type Static = "static" :> Raw
type Frontend = Static
  :<|> Get '[HTML] RawHtml

type API = "api" :> "v1" :> (
  Servant.Auth.Server.Auth '[Cookie] Token :> (Protected :<|> MaybeUser)
  :<|> Servant.Auth.Server.Auth '[UserCookie] (Entity User) :> WithUser
  :<|> Servant.Auth.Server.Auth '[AuthorizeAdmin] (Entity User) :> Admin
  :<|> Unprotected
  )
  :<|> Frontend
