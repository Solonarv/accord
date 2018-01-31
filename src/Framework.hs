{-# LANGUAGE
    LambdaCase
    #-}
module Framework
  ( module Reader
  , module State
  , AccordStack
  , AccordState(..)
  , AccordConfig(..)
  , retrieveAccordConfig
  , initAccordState
  ) where

import Control.Monad.Except
import Control.Monad.Reader as Reader
import Control.Monad.State  as State

import OAuth2.Discord.Token

type AccordStack = StateT AccordState (ReaderT AccordConfig IO)

data AccordState = AccordState

data AccordConfig = AccordConfig
  { aconfigOAuth :: Token
  }

retrieveAccordConfig :: IO AccordConfig
retrieveAccordConfig = do
  token <- retrieveOAuth2Token defaultOAuthCfg
  return $ AccordConfig {
    aconfigOAuth = token
  }

initAccordState :: ReaderT AccordConfig IO AccordState
initAccordState = return AccordState