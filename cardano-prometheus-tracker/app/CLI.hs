module CLI (
    Command (..)
  , getOpts
  ) where

import           Data.List.NonEmpty                (NonEmpty)
import           Options.Applicative

import           Network.HTTP.PrometheusTracker.Types


data Command =
      CScrape           !ScrapeConfig
    | CParse
    | CSample           !String
    | CSummary          !FilePath
    | CCompare          !FilePath   !FilePath
    | CNames            !FilePath
    | CPackage          !FilePath
    | CPlot             !(NonEmpty String)        -- not implemented: create a plot for specified metrics
    deriving Show


getOpts :: IO Command
getOpts = execParser $ info (parseCLI <**> helper) (fullDesc <> progDesc "Track Cardano Prometheus metrics")

parseCLI :: Parser Command
parseCLI = subparser $ mconcat
  [ op "scrape" "Scrape a Prometheus URL"
      (CScrape <$> parseScrapeConfig)
  , op "parse" "Parse all TXTs in $PWD into scrape JSONs; filename must contain timestamp"
      (pure CParse)
  , op "summarize" "Join all scraped JSONs in $PWD into a summary"
      (CSummary <$> parseOutFileName "output JSON file")
  , op "compare" "Print comparison between two summaries to stdout"
      (CCompare <$> parseSummaryFileName "FILE1" <*> parseSummaryFileName "FILE2")
  , op "names" "List all metrics names observed in summary to stdout"
      (CNames <$> parseSummaryFileName "FILE")
  , op "package" "Package scraped JSONs in all immediate subdirs of $PWD into a CBOR blob"
      (CPackage <$> parseOutFileName "output CBOR file")      
  , op "sample" "Scrape once and dump the result to stdout"
      (CSample <$> parseURL)
  ]
  where
    op :: String -> String -> Parser a -> Mod CommandFields a
    op c descr p =
     command c $ info (p <**> helper) $
       mconcat [ progDesc descr ]

    parseScrapeConfig :: Parser ScrapeConfig
    parseScrapeConfig = ScrapeConfig <$> parseDelay <*> parseURL

    parseDelay :: Parser Int
    parseDelay = option auto
      (mconcat
        [ short 'd'
        , long "delay"
        , value 15
        , showDefault
        , help "delay (seconds) between scrapes"
        ])

    parseOutFileName :: String -> Parser FilePath
    parseOutFileName helpText = strArgument
      (mconcat
        [ metavar "FILE"
        , help helpText
        ])

    parseSummaryFileName :: String -> Parser FilePath
    parseSummaryFileName meta = strArgument
      (mconcat
        [ metavar meta
        , action "file"
        , help "summary JSON"
        ])

    parseURL :: Parser String
    parseURL = strArgument
      (mconcat
        [ metavar "URL"
        , help "Prometheus URL, either from a node (legacy) or cardano-tracer (new)"
        ])
