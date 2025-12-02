{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans #-}

module Network.HTTP.PrometheusTracker.CBOR where

import           Codec.Serialise
import           Control.Applicative
import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Map.Strict as Map (Map, size)
import           Data.Text (Text, unpack)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics (Generic)


data NumericValue =
    NVInt       Int
  | NVDouble    Double
  deriving (Generic, Show, Serialise)

instance ToJSON NumericValue where
  toJSON = \case { NVInt i -> toJSON i; NVDouble d -> toJSON d }

instance FromJSON NumericValue where
  parseJSON v = (NVInt <$> parseJSON v) <|> (NVDouble <$> parseJSON v)


data Snapshot = Snapshot
  { singletonLabel  :: Text
  , timeStamp       :: POSIXTime
  , scrape          :: Map Text NumericValue
  }
  deriving (Generic, Serialise)

instance Show Snapshot where
  show (Snapshot l t s) = "Snapshot{" ++ unpack l ++ "} @ " ++ show t ++ ", entries: " ++ show (Map.size s)

instance Serialise POSIXTime where
  encode = encode . toInteger . floor
  decode = fromInteger <$> decode


readFileSnapshots :: FilePath -> IO [Snapshot]
readFileSnapshots = readFileDeserialise

-- can be used with Data.List.sortBy
snapshotOrd :: Snapshot -> Snapshot -> Ordering
snapshotOrd a b =
     singletonLabel a `compare` singletonLabel b
  <> timeStamp a      `compare` timeStamp b
