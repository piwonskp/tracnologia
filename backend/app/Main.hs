{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Main where

import Control.Arrow
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Network.Wai.Handler.Warp (defaultSettings, setPort, run)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Database.Persist.Postgresql (runMigration)
import Servant
import Servant.Auth.Server
import System.Environment (getEnv)

import API (API(..))
import Db
import Handlers
import Models


-- Development uses separate container to serve js, production uses same origin
addCors =  cors (const $ Just corsPolicy)
  where
    corsPolicy = simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type", "Set-Cookie",
                              "X-XSRF-TOKEN"],
      corsOrigins = Just (["https://localhost:3000", "https://127.0.0.1:3000",
                           "https://localhost:8000", "https://127.0.0.1:8000"],
                           True)
      }
runTLS' port = runTLS tlsOpts (setPort port defaultSettings)
  where
    tlsOpts = tlsSettings "certs/localhost.crt" "certs/localhost.key"

runServer run mkServer = curry $ run *** mkServer >>> uncurry ($)
forEnv "development" = runServer runTLS' $ addCors . logStdoutDev
forEnv "production" = runServer run logStdout


cookieSettings = defaultCookieSettings {
  cookieXsrfSetting = Just (defaultXsrfCookieSettings {xsrfExcludeGet = True})
  }

main :: IO ()
main = do
  runSql $ runMigration migrateAll
  jwtCfg <- defaultJWTSettings <$> readKey "secretkey"
  haskellEnv <- getEnv "HASKELL_ENV"
  port <- read <$> getEnv "PORT"
  let cfg = cookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy API
      runForEnv = forEnv haskellEnv

  runForEnv port $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)

