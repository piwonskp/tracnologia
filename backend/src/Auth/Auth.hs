{-# LANGUAGE DataKinds, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, UndecidableInstances #-}
module Auth.Auth where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Database.Persist
import Servant.Auth.Server
import Servant.Auth.Server.Internal.Class
import Servant.Auth.Server.Internal.Types
import Servant.Auth.Server.Internal.Cookie

import Db
import Models (User, userIsAdmin)

type Token = Key User
instance ToJWT Token
instance FromJWT Token

instance ToJWT (Entity User) where
  encodeJWT = encodeJWT . entityKey

data AuthorizeAdmin

instance IsAuth AuthorizeAdmin (Entity User) where
  type AuthArgs AuthorizeAdmin = '[CookieSettings, JWTSettings]
  runAuth _ _ = \c jwt -> authorizeAdmin =<< cookieAuthCheck c jwt

authorizeAdmin userId = (maybe mzero checkAdmin) =<< fetch
  where
    fetch = liftIO $ runSql $ get userId
    checkAdmin usr | userIsAdmin usr = return $ Entity userId usr
                   | otherwise = mzero


data UserCookie

extractUser userId = (maybe mzero (return . Entity userId)) =<< fetch
  where
    fetch = liftIO $ runSql $ get userId

instance IsAuth UserCookie (Entity User) where
  type AuthArgs UserCookie = '[CookieSettings, JWTSettings]
  runAuth _ _ = \c jwt -> extractUser =<< cookieAuthCheck c jwt
