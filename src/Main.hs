{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.IORef

import           Graphics.UI.Gtk

import           Accord.ArgParse
import           Accord.Config
import           Accord.Environment
import           Accord.State
import           OAuth2.Discord.Token

main :: IO ()
main = do
  args <- initGUI
  env <- initAccordEnv args
  window <- windowNew
  widgetShowAll window
  mainGUI

retrieveAccordConfig :: AccordOptions -> IO AccordConfig
retrieveAccordConfig opts = return AccordConfig

initAccordState :: AccordOptions -> AccordConfig -> IO AccordState
initAccordState _ _ = return ServerSelect

initAccordEnv :: [String] -> IO AccordEnv
initAccordEnv args = do
  opts <- parseArgs args
  config <- retrieveAccordConfig opts
  configRef <- newIORef config
  token <- case optToken opts of
    RawToken tok -> mkEphemeralToken tok
    TokenFromFile forceReauth saveToken file ->
      retrieveOAuth2Token OAuthCfg
        { oauthTokenFile   = file
        , oauthAppKeyFile  = defaultAppKeyFile
        , oauthSaveToken   = saveToken
        , oauthForceReauth = forceReauth
        }
  tokenRef <- newIORef token
  stateRef <- initAccordState opts config >>= newIORef
  heartbeatRef <- newIORef $ error "uh oh, need to implement heartbeat thread!"
  return $ AccordEnv
    { aeConfig = configRef
    , aeState = stateRef
    , aeAuthToken = tokenRef
    , aeHeartbeatThread = heartbeatRef
    }
