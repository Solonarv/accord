{-# LANGUAGE
    RecordWildCards, NamedFieldPuns,
    OverloadedStrings
    #-}
module OAuth2.OAuthListener where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (join)
import System.IO

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

data OAuthListenerConfig = OAuthListenerConfig
  { oalistenPort :: Port
  , oalistenBadRequest :: FilePath
  , oalistenCodeReceived :: FilePath
  }

defaultListenerConfig :: OAuthListenerConfig
defaultListenerConfig = OAuthListenerConfig {
    oalistenPort = 5000,
    oalistenBadRequest = "resources/oauth/bad-request.html",
    oalistenCodeReceived = "resources/oauth/code-received.html"
  }

startOAuthListener :: OAuthListenerConfig -> MVar Text -> IO ThreadId
startOAuthListener cfg@OAuthListenerConfig{oalistenPort} outvar = do
  putStrLn $ "Starting OAuth2 callback listener on port " ++ show oalistenPort
  threadID <- forkIO $ run oalistenPort $ oauthListener cfg outvar
  putStrLn $ "Listener started with " ++ show threadID
  return threadID

oauthListener :: OAuthListenerConfig -> MVar Text -> Application
oauthListener OAuthListenerConfig{..} outvar = app where
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
                putMVar outvar $ decodeUtf8 tok
                respond $ responseFile ok200 [("Content-Type", "text/html")] oalistenCodeReceived Nothing
        _ -> respond notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"