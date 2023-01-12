{-# LANGUAGE OverloadedStrings #-}

module  Cardano.Benchmarking.Publish.DBSchema
        ( module Cardano.Benchmarking.Publish.DBSchema
        , module Contra
        ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson
import           Data.ByteString.Char8 as BS (ByteString, null, unpack)
import           Data.Functor.Contravariant as Contra ((>$<))
import           Data.List (sort)
import           Data.Text as T (Text, intercalate, snoc)
import           Data.Text.Encoding as T (decodeLatin1, encodeUtf8)
import           Data.Tuple.Extra

import           Hasql.Connection as DB
import           Hasql.Decoders as Dec hiding (timestamp)
import           Hasql.Encoders as Enc hiding (timestamp)
import           Hasql.Session as DB
import           Hasql.Statement as DB (Statement (Statement))

import           Cardano.Benchmarking.Publish.Types


newtype DBSchema  = DBSchema BS.ByteString
newtype SqlSource = SqlSource BS.ByteString

instance Show DBSchema where
  show (DBSchema s) = BS.unpack s

instance FromJSON DBSchema where
  parseJSON v
    = DBSchema . encodeUtf8 <$> parseJSON v

instance FromJSON SqlSource where
  parseJSON v
    = SqlSource . encodeUtf8 <$> parseJSON v


liftDBRun :: MonadIO m => DB.Session a -> Connection -> ExceptT String m a
liftDBRun session conn
  = do
    result <- liftIO $ session `DB.run` conn
    firstExceptT show $ hoistEither result

setSearchPath :: DBSchema -> ByteString
setSearchPath (DBSchema schemaName)
  = "SET search_path TO " <> schemaName <> ";\n"

-- bootstraps schema with empty tables onto a DB
-- is destructive: drops a possible pre-existing schema with all data
bootstrap :: SqlSource -> DBSchema -> Connection -> ExceptT String IO ()
bootstrap (SqlSource tableSql) schema@(DBSchema schemaName) conn
  = let
      script :: DB.Session ()
      script = DB.sql $ preamble <> setSearchPath schema <> tableSql

    in liftDBRun script conn
  where
    preamble :: ByteString
    preamble =
      "DROP SCHEMA IF EXISTS " <> schemaName <> " CASCADE;\n\
      \CREATE SCHEMA " <> schemaName <> ";\n\
      \COMMENT ON SCHEMA " <> schemaName <> " IS 'This schema provides benchmarking cluster run data';\n"

-- updates API-facing views on the DB without touching any tables or stored data
-- and grants read access of all views defined to the specified role
-- is non-destructive
updateViews :: SqlSource -> DBSchema -> ByteString -> Connection -> ExceptT String IO [Text]
updateViews (SqlSource viewSql) schema@(DBSchema schemaName) anonRole conn
  = do
      let
        script :: DB.Session ()
        script = DB.sql $ setSearchPath schema <> viewSql

      _ <- liftDBRun script conn

      views <- liftDBRun getViews conn
      let
        sch = T.decodeLatin1 schemaName `T.snoc` '.'
        commaSep = T.intercalate "," $ (sch <>) <$> views
        commaSepBS = T.encodeUtf8 commaSep

      _ <- liftDBRun (grant commaSepBS) conn
      _ <- postgrestNotify conn
      pure $ sort views
  where
    getViews :: DB.Session [Text]
    getViews = statement () $
      Statement queryViewNames Enc.noParams (rowList decText) False
      where
        queryViewNames =
          "SELECT viewname FROM pg_catalog.pg_views WHERE schemaname='" <> schemaName <> "'\
          \ UNION SELECT matviewname FROM pg_catalog.pg_matviews WHERE schemaname='" <> schemaName <> "'"

    grant :: ByteString -> DB.Session ()
    grant commaSep =
      let toAnon = " TO " <> anonRole <> ";\n"
      in DB.sql $
        "GRANT USAGE ON SCHEMA " <> schemaName <> toAnon
        <> if BS.null commaSep then mempty else "GRANT SELECT ON " <> commaSep <> toAnon

-- if there has been any change in DB object definitions relevant to PostgREST for
-- serving the API (views, in this case), we need to notify it to rebuild its schema cache
postgrestNotify :: Connection -> ExceptT String IO ()
postgrestNotify
  = liftDBRun (DB.sql "NOTIFY pgrst,'reload schema';")

-- encoder for table 'cluster_run'
encClusterRun :: Params MetaStub
encClusterRun
  =  (profile   >$< param (Enc.nonNullable Enc.text))
  <> (commit    >$< param (Enc.nonNullable Enc.text))
  <> (timestamp >$< param (Enc.nonNullable Enc.timestamptz))

-- encoder for table 'run_info'
encRunInfo :: Params (Int, ByteString)
encRunInfo
  =  (snd                >$< param (Enc.nonNullable Enc.jsonbBytes))
  <> (fromIntegral . fst >$< param (Enc.nonNullable Enc.int4))

-- encoder for table 'run_result'
encRunResult :: Params (Int, Maybe ByteString, Maybe ByteString)
encRunResult
  =  (snd3                >$< param (Enc.nullable Enc.jsonbBytes))
  <> (thd3                >$< param (Enc.nullable Enc.jsonbBytes))
  <> (fromIntegral . fst3 >$< param (Enc.nonNullable Enc.int4))

-- decoder for table 'cluster_run' (including id and published status)
decClusterRun :: Row (Int, MetaStub, Bool)
decClusterRun
  = (,,) <$> decInt4 <*> decMetaStub <*> decBool


-- convenience definitions

decInt4 :: Row Int
decInt4 = fromIntegral <$> (column . Dec.nonNullable) Dec.int4

decText :: Row Text
decText = (column . Dec.nonNullable) Dec.text

decBool :: Row Bool
decBool = (column . Dec.nonNullable) Dec.bool

decMetaStub :: Row MetaStub
decMetaStub
  = MetaStub
    <$> decText
    <*> decText
    <*> (column . Dec.nonNullable) Dec.timestamptz
