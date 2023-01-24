{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module  Cardano.Benchmarking.Publish.Types where

import           Control.Applicative ((<|>))
import           Data.ByteString.Char8 as BS (ByteString, unpack)
import           Data.Text as T (Text, take, unpack)
import           Data.Text.Encoding as T (decodeLatin1, encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.System

import           Data.Aeson as Aeson


newtype DBSchema  = DBSchema BS.ByteString
newtype SqlSource = SqlSource BS.ByteString

instance Show DBSchema where
  show (DBSchema s) = BS.unpack s

instance FromJSON DBSchema where
  parseJSON v
    = DBSchema . encodeUtf8 <$> parseJSON v

instance Show SqlSource where
  show (SqlSource s) = BS.unpack s

instance FromJSON SqlSource where
  parseJSON v
    = SqlSource . encodeUtf8 <$> parseJSON v

instance ToJSON SqlSource where
  toJSON (SqlSource s)
    = toJSON $ decodeLatin1 s


data ClusterRun a
  = ClusterRun {
      runMeta        :: !a
    , runBlockProp   :: !(Maybe a)
    , runClusterPerf :: !(Maybe a)
    , metaStub       :: !MetaStub
    }
    deriving (Functor)

instance FromJSON a => FromJSON (ClusterRun a) where
  parseJSON
    = withObject "ClusterRun" $ \o ->
      ClusterRun
        <$> o .: "meta"
        <*> o .:? "blockprop"
        <*> o .:? "clusterperf"
        <*> o .: "meta"

instance ToJSON a => ToJSON (ClusterRun a) where
  toJSON ClusterRun{..}
    = object [
          "meta"        .= runMeta
        , "blockprop"   .= runBlockProp
        , "clusterperf" .= runClusterPerf
        ]

-- we extract only very few attributes from meta.json to perform
-- a necessary minimum of data normalization to describe a run
data MetaStub
  = MetaStub {
      profile   :: Text
    , commit    :: Text
    , timestamp :: UTCTime
    }

instance Show MetaStub where
  show (MetaStub a b c)
    = concat [T.unpack (T.take 6 b), "--", show c, "--", T.unpack a]

instance FromJSON MetaStub where
  parseJSON v
    = parseFromMetaJSON v <|> parseFromCLI v
    where
      parseFromMetaJSON
        = withObject "MetaStub" $ \o_ -> do
          o <- o_ .: "meta"
          pins <- o .: "pins"
          MetaStub
            <$> o .: "profile"
            <*> pins .: "cardano-node"
            <*> (systemToUTCTime <$> o .: "timestamp")
      parseFromCLI
        = withObject "MetaStub" $ \o ->
          MetaStub
            <$> o .: "profile"
            <*> o .: "commit"
            <*> (systemToUTCTime <$> o .: "timestamp")
