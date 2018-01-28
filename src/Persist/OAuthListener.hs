{-# LANGUAGE
    RecordWildCards, NamedFieldPuns,
    OverloadedStrings
    #-}
module Persist.OAuthListener where

import Control.Concurrent (forkIO)
import Control.Monad (join)
import System.IO

import Data.ByteString (hPut)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

data OAuthListenerConfig = OAuthListenerConfig
  { oalistenPort :: Port
  , oalistenOutputFile :: FilePath
  , oalistenBadRequest :: FilePath
  , oalistenCodeReceived :: FilePath
  }

startOAuthListener :: OAuthListenerConfig -> IO ()
startOAuthListener cfg@OAuthListenerConfig{oalistenPort} = do
  putStrLn $ "Starting OAuth2 callback listener on port " ++ show oalistenPort
  threadID <- forkIO $ run oalistenPort $ oauthListener cfg
  putStrLn $ "Listener started with " ++ show threadID

oauthListener :: OAuthListenerConfig -> Application
oauthListener OAuthListenerConfig{..} = app where
  app request respond = let
      qmap = queryString request
      code = join $ lookup "code" qmap
    in case rawPathInfo request of
        "/oauth2-callback" ->
          case code of
            Nothing -> do
                putStrLn $ "Invalid request: " ++ show request
                respond $ responseFile badRequest400 [("Content-Type", "text/html")] oalistenBadRequest Nothing
            Just tok -> do
                putStrLn $ "Received OAuth2 token: " ++ show tok
                withFile oalistenOutputFile WriteMode $ \h -> hPut h tok
                respond $ responseFile ok200 [("Content-Type", "text/html")] oalistenCodeReceived Nothing
        _ -> respond notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"