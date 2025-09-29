{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

module Network.HTTP.PrometheusTracker
       ( scrapeWhileValid, scrapeOnce
       , module ReExport
       ) where

import           Control.Applicative
import           Control.Concurrent                     (threadDelay)
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Either
import qualified Data.Map.Strict                        as M
import           Data.Maybe
import           Data.Text                              as T (Text)
import qualified Data.Text.Lazy                         as TL
import qualified Data.Text.Lazy.Encoding                as TL (decodeUtf8')
import qualified Data.Text.Lazy.Read                    as TL
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client
import           System.Directory                       (removeFile)
import           System.Exit                            (die)

import           Network.HTTP.PrometheusTracker.Summary as ReExport
import           Network.HTTP.PrometheusTracker.Types
import           Network.HTTP.PrometheusTracker.Utils


parsePrometheus :: TL.Text -> MetricsMap
parsePrometheus exposition = MM (M.fromList $ catMaybes ls)
  where
    ls = [ parseMetricsKV line
            | l <- TL.lines exposition
            , let line = dropCommentsAndTrim l
            , (not . TL.null) line
         ]
    dropCommentsAndTrim = TL.dropAround isSpace . TL.takeWhile (/= '#')

parseMetricsKV :: TL.Text -> Maybe (Text, MetricsValue)
parseMetricsKV (stripLabelInfix -> l) =
    case TL.words l of
      key:val:_ ->
        let
          key' = TL.toStrict $ fromMaybe key (TL.stripPrefix "cardano_node_metrics_" key)

          asInt = case TL.decimal val of
            Right (i, rest) | TL.null rest -> Just $ MVInt i
            _                              -> Nothing
          asDouble = case TL.double val of
            Right (d, rest) | TL.null rest -> Just $ MVDouble d
            _                              -> Nothing
          asText = Just $ MVText $ TL.toStrict val
        in (,) key' <$> asum [ asInt, asDouble, asText ]
      _ -> Nothing

stripLabelInfix :: TL.Text -> TL.Text
stripLabelInfix t = pref <> rest
  where
    (pref, suff) = TL.breakOn "{" t
    (_, rest_)   = TL.breakOn "}" suff
    rest         = maybe "" snd (TL.uncons rest_)

scrapeOnce :: Manager -> String -> IO MetricsMap
scrapeOnce manager scrapeUrl =
    go `catch` handler
  where
    handler (SomeException e) = do
      print e
      die "\n--> assuming no more data will follow; quitting..."

    go = parseUrlThrow scrapeUrl >>= \request -> do
      response <- httpLbs request manager
      pure $ parsePrometheus $ fromRight TL.empty $ TL.decodeUtf8' $ responseBody response

scrapeWhileValid :: Manager -> ScrapeConfig -> IO ()
scrapeWhileValid manager ScrapeConfig{..} =
    go `catch` handler
  where
    handler (SomeException e) = do
      print e
      putStrLn "\n--> assuming no more data will follow; quitting..."

    delay = threadDelay $ scrapeSeconds * 1_000_000

    go = parseUrlThrow scrapeUrl >>= \request ->
      let
        loop 3 = fail "3 empty scrapes in a row"
        loop n = do
          response <- httpLbs request manager

          let parse = parsePrometheus $ fromRight TL.empty $ TL.decodeUtf8' $ responseBody response

          if mmNull parse
            then delay >> loop (n + 1)
            else do
              now <- round <$> getPOSIXTime
              let
                  fn = "scrape-" ++ show (now :: Int) ++ ".json"
              writeFilePretty fn parse
              delay
              loop 0

      in cleanup >> loop (0 :: Int)

cleanup :: IO ()
cleanup = do
  previousScrapes <- listScrapeFiles
  unless (null previousScrapes) $ do
    putStrLn $ "--> cleaning up " ++ show (length previousScrapes) ++ " previous scrape files"
    mapM_ removeFile previousScrapes
