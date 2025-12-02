{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Network.HTTP.PrometheusTracker
       ( scrapeWhileValid
       , scrapeOnce
       , packageToCBOR
       , parseScrapeTxtFiles
       , module ReExport
       ) where

import           Codec.Serialise.IO (writeFileSerialise)
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad
import           Data.Aeson (eitherDecodeFileStrict')
import           Data.Char
import           Data.Either
import           Data.List (sortBy)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text as T (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8')
import qualified Data.Text.Lazy.IO as TL (readFile)
import qualified Data.Text.Lazy.Read as TL
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client
import           System.Directory
import           System.Exit (die)
import           System.FilePath (takeDirectory, (</>))

import           Network.HTTP.PrometheusTracker.CBOR
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

parseScrapeTxtFiles :: IO ()
parseScrapeTxtFiles = do
  txts <- listScrapeTxts "."
  if null txts
    then putStrLn "--> no matching scrape TXTs found"
    else do
      cleanup
      forM_ txts $ \txt ->
        let fn = "scrape-" ++ show (fromJust $ timestampOfTxt txt) ++ ".json"
        in TL.readFile txt >>= writeFilePretty fn . parsePrometheus
      putStrLn $ "--> created " ++ show (length txts) ++ " scrape-*.json files"

cleanup :: IO ()
cleanup = do
  previousScrapes <- listScrapeFiles "."
  unless (null previousScrapes) $ do
    putStrLn $ "--> cleaning up " ++ show (length previousScrapes) ++ " previous scrape files"
    mapM_ removeFile previousScrapes

packageToCBOR :: FilePath -> IO ()
packageToCBOR file = do
  exists <- doesFileExist file
  if not exists
    then packageToCBORWrite file
    else do
      snapshots <- readFileSnapshots file
      mapM_ print $ sortBy snapshotOrd snapshots
      putStrLn $ "--> total snapshots: " ++ show (length snapshots)

packageToCBORWrite :: FilePath -> IO ()
packageToCBORWrite outFile = do
  subdirs <- listDirectory "." >>= filterM doesDirectoryExist
  files <- concat <$> forM subdirs
    (\dir -> map (dir </>) <$> listScrapeFiles dir)
  mapM loadToSnapshot files >>= writeFileSerialise outFile
  where
    loadToSnapshot f =
      let
        timeStamp      = maybe (error $ "should never happen: " ++ f) (fromInteger . toInteger) $ timestampOfScrape f
        singletonLabel = T.pack $ takeDirectory f
      in do
        eitherDecodeFileStrict' f >>= \case
          Right scrape -> pure Snapshot{..}
          Left err     -> error err
