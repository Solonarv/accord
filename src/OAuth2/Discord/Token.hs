{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
module OAuth2.Discord.Token where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                (when)
import           Data.List                    (intercalate)
import           Data.Monoid                  ((<>))
import           GHC.Generics
import           Text.Printf

import           Data.Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Time
import           Network.HTTP.Req

import           Network.HTTP.ThrowingIO
import           OAuth2.Discord.OAuthListener

default (Int, Text)

data OAuthCfg = OAuthCfg
  { oauthTokenFile   :: FilePath
  , oauthAppKeyFile  :: FilePath
  , oauthSaveToken   :: Bool
  , oauthForceReauth :: Bool
  } deriving Show

defaultAppKeyFile :: FilePath
defaultAppKeyFile = "local/clientkeys.json"

data ClientKeys = ClientKeys
  { clientID     :: Text
  , clientSecret :: Text
  } deriving (Show, Generic)
instance ToJSON ClientKeys
instance FromJSON ClientKeys

-- Text is the wrong type for this, but ByteString (which is the correct type)
-- does not have Aeson instances for $reasons.
data Token = Token
  { tokenAccess  :: Text
  , tokenExpiry  :: UTCTime
  , tokenRefresh :: Text
  } deriving (Show, Generic)
instance ToJSON Token
instance FromJSON Token

isEphemeral :: Token -> Bool
isEphemeral = Text.null . tokenRefresh

data TokenResponse = TokenResponse
  { access_token  :: Text
  , expires_in    :: NominalDiffTime
  , refresh_token :: Text
  } deriving (Show, Generic)
instance FromJSON TokenResponse

toToken :: UTCTime -> TokenResponse -> Token
toToken now (TokenResponse access expiry refresh)
  = Token access (addUTCTime expiry now) refresh

mkEphemeralToken :: Text -> IO Token
mkEphemeralToken tok = do
  now <- getCurrentTime
  return $ Token
    { tokenAccess = tok
    , tokenExpiry = now
    , tokenRefresh = ""
    }

retrieveOAuth2Token :: OAuthCfg -> IO Token
retrieveOAuth2Token cfg@OAuthCfg{..} = do
    token <- if oauthForceReauth
      then newToken
      else decodeStrict <$> BS.readFile oauthTokenFile >>= \case
        Just tok -> maybeRefresh tok
        Nothing -> newToken
    when oauthSaveToken $ BS.writeFile oauthTokenFile $ BSL.toStrict $ encode token
    return token
  where
    maybeRefresh tok@Token{..} = do
      now <- getCurrentTime
      if diffUTCTime tokenExpiry now < tokenRefreshThreshold
        then do
          keys <- getClientKeys cfg
          tokenRequest keys
              $  "grant_type"    =: "refresh_token"
              <> "refresh_token" =: tokenRefresh
          else return tok
    newToken = do
      authUrl <- mkOAuthURL cfg
      putStrLn $ "No OAuth2 token was found to connect to your Discord account.\n\
                 \Please visit this URL to allow Accord to access your account.\n"
                 ++ authUrl
      -- WARNING: here be dragons. Messing about with forking, MVar's and killing threads.
      outvar <- newEmptyMVar
      tid <- startOAuthListener defaultListenerConfig {oalistenPort = localPort} outvar
      code <- takeMVar outvar
      killThread tid
      keys <- getClientKeys cfg
      tokenRequest keys
          $  "grant_type"    =: "authorization_code"
          <> "code"          =: code

tokenRequest :: ClientKeys -> FormUrlEncodedParam -> IO Token
tokenRequest ClientKeys{..} kv = do
    now <- getCurrentTime
    throwingHttp $ toToken now . responseBody <$> req
      POST
      (baseOAuthURL /: "token")
      (ReqBodyUrlEnc $ defHeaders <> kv)
      jsonResponse
      mempty
  where
    defHeaders = "client_id"     =: clientID
              <> "client_secret" =: clientSecret
              <> "redirect_uri"  =: redirectURI

mkOAuthURL :: OAuthCfg -> IO String
mkOAuthURL cfg = do
    ClientKeys{clientID} <- getClientKeys cfg
    return $ renderUrl $ Text.unpack clientID
  where
    renderUrl clientID = printf "%v/authorize?%v"
      baseOAuthURLRaw
      (paramsRendered clientID)
    paramsRendered clientID = intercalate "&"
      $ map (uncurry $ printf "%v=%v")
      $ params clientID
    params :: String -> [(String, String)]
    params clientID =
      [ ("client_id", clientID)
      , ("redirect_uri", redirectURI)
      , ("response_type", "code")
      , ("scope", oAuth2Scopes)
      ]

getClientKeys :: OAuthCfg -> IO ClientKeys
getClientKeys OAuthCfg{..} = do
  keys <- decodeStrict <$> BS.readFile oauthAppKeyFile
  case keys of
    Just k -> return k
    Nothing -> fail $
      "Something's wrong with your API key file! It should be located at " ++ oauthAppKeyFile

-- | Everything except:
--  - scopes that are only relevant for bots (this isn't a bot)
--  - local RPC (this is a standalone client, we're not interfacing with the exsting client)
--  - webhooks, we don't need those
oAuth2Scopes :: String
oAuth2Scopes = "connections email identify guilds gdm.join messages.read rpc.api"

redirectURI :: String
redirectURI = printf "http://localhost:%v/oauth2-callback" localPort

localPort :: Int
localPort = 5000

baseOAuthURL :: Url 'Https
baseOAuthURL = https "discordapp.com" /: "api" /: "oauth2"

baseOAuthURLRaw :: String
baseOAuthURLRaw = "https://discordapp.com/api/oauth2"

-- | If an existing token has less than this amount of time left,
--   it will be refreshed. Currently hard-coded to one hour.
tokenRefreshThreshold :: NominalDiffTime
tokenRefreshThreshold = 3600
