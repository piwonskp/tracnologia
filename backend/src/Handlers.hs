{-# LANGUAGE PartialTypeSignatures, FlexibleContexts, DataKinds #-}
module Handlers where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import qualified Data.ByteString.Lazy as LB
import Servant
import Servant.Auth.Server

import Models (User)
import API
import Auth.User
import Auth.Auth
import Db
import Handlers.Meter
import Handlers.MeterReading
import Handlers.User
import Handlers.Stats

import Servant.Checked.Exceptions


unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = login cs jwts

protected :: Servant.Auth.Server.AuthResult Token -> Server Protected
protected (Servant.Auth.Server.Authenticated userId) =
  userContext userId :<|> meterH
  :<|> listByType :<|> addMeterReading userId :<|> recentReadings userId
  where
    meterH = listMeters userId :<|> addMeter userId
    listByType measureType = listReadings userId measureType
protected _ = throwAll err401

maybeUser :: Servant.Auth.Server.AuthResult Token -> Server MaybeUser
maybeUser (Servant.Auth.Server.Authenticated userId) =
  statsH :<|> userTimelineStats userId
  where
    statsH measureType = dailyStats userId measureType
      :<|> userMonthlyStats userId measureType
maybeUser _ = statsH :<|> timelineStats
  where
    statsH measureType = throwError err401 :<|> monthlyStats measureType

withUser :: Servant.Auth.Server.AuthResult (Entity User) -> Server WithUser
withUser (Servant.Auth.Server.Authenticated user) =
  changePassword user
withUser _ = throwAll err401

admin :: Servant.Auth.Server.AuthResult (Entity User) -> Server Admin
admin (Servant.Auth.Server.Authenticated user) =
  listUsers :<|> getUser :<|> addUser
admin _ = throwAll err401

frontend :: Server Frontend
frontend = serveStatic :<|> serveIndex
  where
    serveStatic :: Server Static
    serveStatic = serveDirectoryFileServer "frontend/static"
    serveIndex :: Handler RawHtml
    serveIndex = do
      index <- liftIO $ LB.readFile "frontend/index.html"
      return $ RawHtml index


server :: CookieSettings -> JWTSettings -> Server API
server cs jwts = (
  userAuth
  :<|> withUser
  :<|> admin
  :<|> unprotected cs jwts
  )
  :<|> frontend
  where
    userAuth userId = protected userId :<|> maybeUser userId
