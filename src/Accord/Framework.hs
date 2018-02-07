{-# LANGUAGE NamedFieldPuns #-}
module Accord.Framework
  ( module Reader
  , module State
  , AccordEnv(..)
  , AccordState(..)
  , AccordConfig(..)
  , initAccordEnv
  ) where

import           Data.IORef

import           Control.Monad.Except
import           Control.Monad.Reader as Reader
import           Control.Monad.State  as State

import           Accord.ArgParse
import           Accord.Config
import           OAuth2.Discord.Token

data AccordEnv = AccordEnv
  { aeAuthToken :: Token
  , aeState     :: IORef AccordState
  , aeConfig    :: IORef AccordConfig
  }

data AccordState = Initializing

retrieveAccordConfig :: AccordOptions -> IO AccordConfig
retrieveAccordConfig opts = return AccordConfig

initAccordState :: AccordOptions -> AccordConfig -> IO AccordState
initAccordState _ _ = return Initializing

initAccordEnv :: [String] -> IO AccordEnv
initAccordEnv args = do
  opts <- parseArgs args
  config <- retrieveAccordConfig opts
  configRef <- newIORef config
  token <- retrieveOAuth2Token OAuthCfg
    { oauthTokenFile = optCfgFile opts
    , oauthAppKeyFile = defaultAppKeyFile
    }
  state0 <- initAccordState opts config >>= newIORef
  return $ AccordEnv
    { aeConfig = configRef
    , aeState = state0
    , aeAuthToken = token
    }
