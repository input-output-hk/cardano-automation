
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module JSONWrapper
    ( AsValue
    , JSONResult(..)
    , JSONWrapper(..)
    , resultError
    , resultSuccess
    , resultAsPlain
    , valueToBS
    )
    where

import           Data.Foldable (toList)
import           GHC.Generics (Generic)
import           Prelude hiding (error)

import           Data.Aeson as Aeson
import           Data.ByteString.Char8 (ByteString, toStrict)
import           Data.Text as T (Text, pack, unpack)

import           Command (Command)


data JSONWrapper
  = JSONWrapper
    { command :: Command
    , schema  :: Maybe String
    , payload :: Maybe Aeson.Value
    }
    deriving (Generic)

data JSONResult
  = JSONResult
    { exitcode  :: Int                          -- 0 is success
    , result    :: Maybe Aeson.Value
    , error     :: Maybe Text
    }
    deriving (Generic)


jsonOptionsUnTaggedSum :: Options
jsonOptionsUnTaggedSum
  = defaultOptions {sumEncoding = ObjectWithSingleField}

instance FromJSON JSONWrapper where
  parseJSON   = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON JSONResult where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum

resultError :: String -> JSONResult
resultError err
  = JSONResult (-1) Nothing (Just $ T.pack err)

resultSuccess :: ToJSON a => a -> JSONResult
resultSuccess res
  = JSONResult 0 (Just $ toJSON res) Nothing

resultAsPlain :: JSONResult -> (String, Int)
resultAsPlain JSONResult{..}
  | exitcode == 0   = (maybe "" go result, 0)
  | otherwise       = (maybe "" T.unpack error, exitcode)
  where
    go :: Aeson.Value -> String
    go (String t)   = T.unpack t
    go (Array a)    = unlines $ map go $ toList a
    go v            = show v

newtype AsValue
  = ValueWrapper {unWrap :: Aeson.Value}
  deriving (FromJSON, ToJSON)

valueToBS :: AsValue -> ByteString
valueToBS
  = toStrict . encode . unWrap
