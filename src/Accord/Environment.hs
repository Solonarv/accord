{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Accord.Environment where

import           Control.Concurrent   (ThreadId)
import           Data.IORef

import           Data.Has
import           Data.Has.TH

import           Accord.Config
import           Accord.State
import           OAuth2.Discord.Token

data AccordEnv = AccordEnv
  { aeAuthToken       :: IORef Token
  , aeState           :: IORef AccordState
  , aeConfig          :: IORef AccordConfig
  , aeHeartbeatThread :: IORef ThreadId
  }

mkHasInstances ''AccordEnv

