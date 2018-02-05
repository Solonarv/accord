{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Accord.ArgParse
  ( AccordOptions(..)
  , TokenOption(..)
  , ShouldForceReAuth, ShouldWriteToken
  , parser, parserInfo, parseArgs
) where

import Data.Semigroup ((<>))

import Options.Applicative
import qualified Data.Text as Text

data AccordOptions = AccordOptions
  { optToken   :: TokenOption
  , optCfgFile :: FilePath
  }

data TokenOption
  = TokenFromFile ShouldForceReAuth ShouldWriteToken FilePath
  | RawToken Text.Text
  deriving (Show, Eq)

type ShouldForceReAuth = Bool
type ShouldWriteToken = Bool

parser :: Parser AccordOptions
parser = do
  optToken <- token
  optCfgFile <- configLocation
  pure AccordOptions{..}

parserInfo :: ParserInfo AccordOptions
parserInfo = info
  (parser <**> helper)
  (  fullDesc
  <> header "Accord - a fast, lightweight Discord client"
  )

parseArgs :: [String] -> IO AccordOptions
parseArgs args = handleParseResult $ execParserPure defaultPrefs parserInfo args

token :: Parser TokenOption
token = rawToken <|> tokenFile
  where
    rawToken = RawToken . Text.pack <$> strOption
      (  long "token"
      <> short 't'
      <> metavar "TOKEN"
      <> help "Use TOKEN to authenticate with the Discord API. \
              \If this option is given, neither -r nor -f may be \
              \specified."
      )
    tokenFile = TokenFromFile 
      <$> switch
        (  short 'r'
        <> long "force-reauth"
        <> help "Obtain a new authentication token even if one is already present."
        )
      <*> switch
        (  long "no-save-token"
        <> help "Do not save the authentication token after obtaining it."
        )
      <*> strOption
        (  long "tokenfile"
        <> short 'f'
        <> metavar "TOKENFILE"
        <> help "Read authentication token from TOKENFILE."
        <> value "local/token.json"
        )

configLocation :: Parser FilePath
configLocation = strOption
  $  long "config"
  <> short 'c'
  <> metavar "CONFIG"
  <> value "local/config.yaml"
  <> help "Specify an alternative configuration file. It is written in YAML, \
          \a superset of JSON."