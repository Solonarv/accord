{-# LANGUAGE LambdaCase #-}
module Main where

import OAuth2.Token

main :: IO ()
main = do
    token <- retrieveOAuth2Token defaultOAuthCfg
    loop
  where
    loop = print "Type `exit` to exit." >> getLine >>= \case
      "exit" -> pure ()
      _ -> loop