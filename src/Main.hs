{-# LANGUAGE LambdaCase #-}
module Main where

import Persist.OAuthListener (startOAuthListener, OAuthListenerConfig(..))

main :: IO ()
main = do
    startOAuthListener oauthListenerCfg
    loop
  where
    loop = print "Type `exit` to exit." >> getLine >>= \case
      "exit" -> pure ()
      _ -> loop
    oauthListenerCfg = OAuthListenerConfig {
      oalistenPort = 5000,
      oalistenOutputFile = "local/oauth-token.txt",
      oalistenBadRequest = "resources/oauth/bad-request.html",
      oalistenCodeReceived = "resources/oauth/code-received.html"
    }