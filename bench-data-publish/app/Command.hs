{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Command
        ( CLICommand (..)
        , Command (..)
        , Config(..)
        , DBCredentials(..)
        , envVarDBPass
        , envVarDBURI
        , parseCommandLine
        ) where

import           Data.Aeson
import           GHC.Generics (Generic)
import           Options.Applicative as Opt


data Command
  = Publish
  | Bootstrap
  | List
  | UpdateViews
  | Import
  | ImportAll
  deriving (Show, Generic)

instance FromJSON Command where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = ObjectWithSingleField}

data CLICommand
  = CLIPublish Bool FilePath
  | CLIImport FilePath
  | CLIImportAll FilePath
  | CLIList
  | CLIBootstrap FilePath
  | CLIUpdateViews FilePath String
  | CLIViaStdin
  deriving Show

data DBCredentials
  = PostgresURI String
  | DBCreds {
        dbName :: String
      , dbUser :: Maybe String
      , dbPass :: Maybe String
      , dbHost :: Maybe String
      , dbPort :: Maybe Int
      }
  | NoCreds
  deriving Show

data Config
  = Config {
        appCommand  :: CLICommand
      , appDB       :: DBCredentials
      , appDBSchema :: Maybe String
      , appForce    :: Bool
      }
  deriving Show

envVarDBPass, envVarDBURI :: String
envVarDBPass    = "BENCH_DATA_PASS"
envVarDBURI     = "BENCH_DATA_PGURI"


parseCommandLine :: IO Config
parseCommandLine =
    Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parseConfig mempty

parseConfig :: Parser Config
parseConfig
  = parseViaStdin
    <|>
    ( Config
    <$> parseCommand
    <*> parseDBCredentials
    <*> optional parseDBSchema
    <*> parseForce
    )
  where
    parseViaStdin = subparser $
        cmdParser "via-stdin"
          (pure $ Config CLIViaStdin NoCreds Nothing False)
          "Expect command and payload as JSON via stdin"
    parseCommand = subparser $ mconcat
      [ cmdParser "import"
          (CLIImport <$> parseRunDirArg)
          "Import/update specified run"
      , cmdParser "import-all"
          (CLIImportAll <$> strArgument (metavar "PATH" <> help "Path containing benchmarking runs" <> completer (bashCompleter "directory")))
          "Import/update all runs contained in directory"
      , cmdParser "list"
          (pure CLIList)
          "List all runs"
      , cmdParser "publish"
          (CLIPublish True <$> parseRunDirArg)
          "Publish specified run to API"
      , cmdParser "unpublish"
          (CLIPublish False <$> parseRunDirArg)
          "Unpublish specified run from API"
      , cmdParser "bootstrap"
          (CLIBootstrap <$> parseFileArg ".sql file containing table definitions")
          "Bootstrap schema onto DB, CLEARING PREVIOUS SCHEMA"
      , cmdParser "update-views"
          (CLIUpdateViews
            <$> parseFileArg ".sql file containing view definitions"
            <*> strArgument (metavar "ROLE" <> help "Anonymous/read-only role on the DB")
          )
          "Only update API facing views for role in the schema, not touching any tables or stored data"
      ]
    cmdParser cmd parser description = command cmd $ info parser $ progDesc description

parseDBCredentials :: Parser DBCredentials
parseDBCredentials =
    postgresUri <|> parseCreds <|> pure NoCreds
  where
    postgresUri
      = PostgresURI <$> strOption
        ( long "pg-uri"
        <> metavar "URI"
        <> help ("postgres[ql]:// URI of DB (default: $" ++ envVarDBURI ++ ")")
        )
    parseCreds
      = DBCreds
        <$> strOption (long "db" <> help "DB name")
        <*> parseCredAttribute 'u' "DB user name (default: <your login name>)"
        <*> parseCredAttribute 'p' ("DB password (default: $" ++ envVarDBPass ++ ")")
        <*> parseCredAttribute 'h' "DB host (default: localhost)"
        <*> optional (option auto $ short 'P' <> help "DB port (default: 5432)")

parseCredAttribute :: Char -> String -> Parser (Maybe String)
parseCredAttribute c h
  = optional $ strOption (short c <> help h)

parseDBSchema :: Parser String
parseDBSchema
  = strOption
    ( short 's'
    <> metavar "SCHEMA"
    <> help "DB schema to use (default: public)"
    )

parseForce :: Parser Bool
parseForce
  = switch
    (short 'f' <> help "Force destructive operations (e.g. bootstrap)")

parseRunDirArg :: Parser FilePath
parseRunDirArg
  = strArgument
    ( metavar "FILE|PATH"
    <> help "Path of a benchmarking run or its meta.json"
    <> completer (bashCompleter "file")
    )

parseFileArg :: String -> Parser FilePath
parseFileArg helpText
  = strArgument
    ( metavar "FILE"
    <> help helpText
    <> completer (bashCompleter "file")
    )
