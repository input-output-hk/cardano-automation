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
  | GetBuiltin
  deriving (Show, Generic)

instance FromJSON Command where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = ObjectWithSingleField}

data CLICommand
  = CLIPublish Bool FilePath
  | CLIImport FilePath
  | CLIImportAll FilePath
  | CLIList
  | CLIBootstrap (Maybe FilePath)
  | CLIUpdateViews String (Maybe FilePath)
  | CLIGetBuiltin FilePath
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

emptyConfig :: CLICommand -> Config
emptyConfig cmd = Config cmd NoCreds Nothing False

parseCommandLine :: IO Config
parseCommandLine =
    Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info parseConfig mempty

parseConfig :: Parser Config
parseConfig
  =     parseCommandWithoutCredentials
    <|> ( Config
        <$> parseCommand
        <*> parseDBCredentials
        <*> optional parseDBSchema
        <*> parseForce
        )
  where
    parseCommandWithoutCredentials = subparser $ mconcat
       [ cmdParser "via-stdin"
          (pure $ emptyConfig CLIViaStdin)
          "Expect command and payload as JSON via stdin"
       , cmdParser "get-builtin"
          (emptyConfig . CLIGetBuiltin <$> strArgument (metavar "NAME" <> help "The built-in .sql to get"))
          "Output a built-in .sql query"
       , commandGroup "CMD1 (without DB credentials):"
       , metavar "CMD1"
       ]

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
          (CLIBootstrap <$> optionalFileArg ".sql table definitions (overriding built-in!)")
          "Bootstrap schema onto DB, CLEARING PREVIOUS SCHEMA"
      , cmdParser "update-views"
          (CLIUpdateViews
            <$> strOption (short 'r' <> metavar "ROLE" <> help "Anonymous/read-only role on the DB")
            <*> optionalFileArg ".sql view definitions (overriding built-in!)"
          )
          "Only update API facing views for role in the schema, not touching any tables or stored data"
       , commandGroup "CMD2 (with DB credentials):"
       , metavar "CMD2"
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

optionalFileArg :: String -> Parser (Maybe FilePath)
optionalFileArg helpText
  = optional $ strArgument
    ( metavar "FILE"
    <> help helpText
    <> completer (bashCompleter "file")
    )
