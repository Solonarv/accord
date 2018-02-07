{-# LANGUAGE NamedFieldPuns #-}
module Accord.Framework
  ( module Reader
  , module State
  , AccordEnv(..)
  , AccordState(..)
  , AccordConfig(..)
  , initAccordEnv
  ) where

import           Control.Concurrent
import           Data.IORef

import           Control.Monad.Except
import           Control.Monad.Reader  as Reader
import           Control.Monad.State   as State
import           Network.Discord.Types

import           Accord.ArgParse
import           Accord.Config
import           OAuth2.Discord.Token

data AccordEnv = AccordEnv
  { aeAuthToken       :: Token
  , aeState           :: IORef AccordState
  , aeConfig          :: IORef AccordConfig
  , aeHeartbeatThread :: IORef ThreadId
  }

data AccordState
  = ServerSelect
  | DirectMsgSelect
  | InDMChannel !Snowflake
  | ServerChannelSelect !Snowflake
  | InServerChannel !Snowflake

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
  state0 <- initAccordState opts config >>= newIORef
  return $ AccordEnv
    { aeConfig = configRef
    , aeState = state0
    , aeAuthToken = token
    }
