{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures, FlexibleContexts #-}

module Auth.User where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Data.ByteString

import Crypto.Scrypt
import Database.Persist.Sql
import Database.Persist.Postgresql
import Servant (Handler, Headers, Header, throwError, err401, err403)
import Servant.Auth.Server

import API
import Models
import Db
import Handlers.Meter

encryptBS :: Pass -> IO ByteString
encryptBS = fmap getEncryptedPass . encryptPassIO'

encrypt :: Pass -> IO Text
encrypt = fmap decodeUtf8 . encryptBS

throwOnNothing maybeInMonad = valOr401 =<< maybeInMonad
valOr401 val = maybe (throwError err401) return val
getUserContext user userId = (UserContext  user) <$> listMeters userId

userContext :: UserId -> Handler UserContext
userContext userId = do
  user <- throwOnNothing $ liftIO $ runSql $ get userId
  getUserContext user userId

login :: CookieSettings -> JWTSettings -> Login
  -> Handler (Headers '[ Header "Set-Cookie" SetCookie,
                         Header "Set-Cookie" SetCookie]
               UserContext)
login cs jwts (Login email password) = do
  user <- liftIO $ runSql $ getBy $ UniqueEmail email
  case user of
    Nothing -> throwError err401
    Just (Entity userId user)-> loginResponse cs jwts user userId password

textToPass = EncryptedPass . encodeUtf8
verify loginPass dbPass = verifyPass' loginPass $ textToPass dbPass

loginResponse cs jwts user userId loginPass =
  applyCookies <*> state
  where
    dbPass = userPassword user
    state = getUserContext user userId
    applyCookies = throwOnNothing maybeApply
    maybeApply = if checkPass then mkApply else (throwError err401)
    mkApply = liftIO $ acceptLogin cs jwts userId
    checkPass = verify loginPass dbPass
