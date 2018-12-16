{-# LANGUAGE LambdaCase, GADTs, DataKinds #-}
module Handlers.User where

import Control.Monad.IO.Class
import Database.Persist
import Servant
import Servant.Checked.Exceptions (Envelope, pureErrEnvelope, pureSuccEnvelope)

import API (NewUser(..), ChangePassword(..))
import Error
import Auth.User
import Db
import Models


listUsers :: Handler [User]
listUsers = liftIO $
  map entityVal <$> (runSql $ selectList ([] :: [Filter User]) [])

getUser :: Key User -> Handler User
getUser userId = (liftIO $ runSql $ get userId) >>= \case
  Nothing -> throwError err404
  Just user -> return user

addUserIO :: NewUser -> IO User
addUserIO (NewUser email password name) = do
  encrypted <- encrypt password
  let user' = user email name encrypted
  runSql $ insert user'
  return user'

addUser :: NewUser -> Handler User
addUser = liftIO . addUserIO

changePassword :: Entity User -> ChangePassword -> Handler NoContent
changePassword (Entity userId user) (ChangePassword current new) =
  if correct then change else throw
  where
    correct = verify current (userPassword user)
    save pass = runSql $ update userId [UserPassword =. pass]
    throw = throwJSONError err401 $ JSONError WrongPassword
    change = do
      encrypted <- liftIO $ encrypt new
      liftIO $ save encrypted
      return NoContent

