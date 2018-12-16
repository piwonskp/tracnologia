{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Error where

import GHC.Generics         (Generic)
import Servant
import Data.Aeson           (ToJSON, encode)
import Network.HTTP.Types   (hContentType)
import Control.Monad.Except (MonadError)


data ErrorType = WrongPassword
  deriving (Generic, Show, Eq, ToJSON)

data JSONError = JSONError
  { error :: ErrorType
  } deriving (Generic, ToJSON)

throwJSONError :: (MonadError ServantErr m, ToJSON a) => ServantErr -> a -> m b
throwJSONError err json = throwError $ err
  { errBody = encode json
  , errHeaders = [ jsonHeader ]
  }
  where
    jsonHeader = ( hContentType
                 , "application/json;charset=utf-8" )
