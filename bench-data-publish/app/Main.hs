{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, hoistEither)
import           Data.Bool (bool)
import           Data.ByteString.Char8 as BS (ByteString, empty, pack, readFile)
import           Data.ByteString.Lazy.Char8 as BSL (getContents, putStr)
import           Data.Maybe (fromJust)

import           System.Directory
import           System.Directory.Extra (listDirectories)
import           System.Environment (lookupEnv)
import           System.Exit
import           System.FilePath
import           System.Posix.User (getLoginName)
import           Text.Printf

import           Data.Aeson as Aeson
import           Hasql.Connection as DB (Connection, Settings, settings)

import           Cardano.Benchmarking.Publish.DBConnection
import           Cardano.Benchmarking.Publish.DBQueries
import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types
import           Command
import           JSONWrapper

import           Paths_bench_data_publish


main :: IO ()
main
  = do
    conf <- parseCommandLine
    dbSettings <- getDBSettings (appDB conf)
    result <- evalCatching conf dbSettings
    case appCommand conf of
      CLIViaStdin -> BSL.putStr $ encode result
      _ ->
        let (msg, code) = resultAsPlain result
        in putStrLn msg >> if code == 0 then exitSuccess else exitWith (ExitFailure code)

toJSONWrapper :: Config -> IO JSONWrapper
toJSONWrapper Config{..}
  = case appCommand of
    CLIViaStdin ->
      BSL.getContents >>= either Prelude.error pure . eitherDecode
    CLIList ->
      wrap List ()
    CLIBootstrap tableSqlFile -> do
      sql <- maybe (show <$> loadBuiltinSql "bench-data-tables.sql") Prelude.readFile tableSqlFile
      wrap Bootstrap (appForce, sql)
    CLIUpdateViews anonRole viewsSqlFile -> do
      sql <- maybe (show <$> loadBuiltinSql "bench-data-views.sql") Prelude.readFile viewsSqlFile
      wrap UpdateViews (anonRole, sql)
    CLIPublish publish target -> do
      eMeta :: Either String Aeson.Value <-
        eitherDecodeFileStrict (normalizeMetaFilePath target)
      case eMeta of
        Left err    -> Prelude.error err
        Right meta  -> wrap Publish (publish, meta)
    CLIImport target -> do
      eRun :: Either String (ClusterRun AsValue) <-
        loadClusterRun target
      case eRun of
        Left err  -> Prelude.error err
        Right run -> wrap Import run
    CLIImportAll targetDir ->
      wrap ImportAll targetDir
    CLIGetBuiltin sqlName ->
      wrap GetBuiltin sqlName
  where
    wrap :: ToJSON a => Command -> a -> IO JSONWrapper
    wrap cmd
      = pure . JSONWrapper cmd appDBSchema . Just . toJSON

evalCatching :: Config -> DB.Settings -> IO JSONResult
evalCatching conf db
  = do
    res <- try $
      eval db =<< toJSONWrapper conf
    pure $ case res of
      Left (SomeException e)  -> resultError (show e)
      Right ok                -> ok

eval :: DB.Settings -> JSONWrapper -> IO JSONResult
eval dbSettings JSONWrapper{..}
  = either errorResult id <$> go
  where
    dbSchema        = DBSchema $ maybe "public" BS.pack schema
    errorResult msg = resultError $ "ERROR: (" ++ show command ++ ") -- " ++ msg

    liftUnwrapPayload Nothing
      = throwE "no payload defined"
    liftUnwrapPayload (Just p)
      = case fromJSON p of
        Error e -> throwE e
        Success v -> pure v

    go
      = case command of
          GetBuiltin  -> runExceptT goNoDB
          _           -> withDB dbSettings (runExceptT . goWithDB)

    goNoDB :: ExceptT String IO JSONResult
    goNoDB
      = case command of
          GetBuiltin -> do
            sqlName <- liftUnwrapPayload payload
            resultSuccess <$> getBuiltInSql sqlName

    goWithDB :: Connection -> ExceptT String IO JSONResult
    goWithDB conn
      = case command of
        Bootstrap -> do
          (force, tableSql) <- liftUnwrapPayload payload
          if not force
          then throwE "requires -f (force); it is a destructive operation"
          else do
            bootstrap tableSql dbSchema conn
            pure $ resultSuccess $
              "successfully bootstrapped schema: '" ++ show dbSchema ++ "'"

        UpdateViews -> do
          (anonRole, viewSql) <- liftUnwrapPayload payload
          views <- updateViews viewSql dbSchema (BS.pack anonRole) conn
          pure $ resultSuccess $
            "views exposed to API (role '" ++ anonRole ++ "'): " ++ show views ++
            "\nNB. if any view has been *renamed*, please drop the old one manually from the DB!"

        Import -> do
          run <- liftUnwrapPayload payload
          storeRunsToDB dbSchema conn $ Right run
          pure $ resultSuccess Aeson.Null

        ImportAll -> do
          targetDir <- liftUnwrapPayload payload
          runMetas <- liftIO $ searchRuns targetDir
          unless (null runMetas) $
            storeRunsToDB dbSchema conn (Left runMetas)
          pure $ resultSuccess $
            length runMetas

        List -> do
          runs <- liftDBRun (dbGetRuns dbSchema) conn
          let
            isPublished p = bool '-' '+' p : "published"

            msg :: [String]
            msg =  [printf "%3i %s -- %s" ix (isPublished publ) (show meta) | (ix, meta, publ) <- runs]
                ++ [printf "---- %i runs" (length runs)]

          pure $ resultSuccess msg

        Publish -> do
          (publish, meta) <- liftUnwrapPayload payload
          found <- liftDBRun (dbPublishRun dbSchema meta publish) conn
          pure $ resultSuccess $
            if found
            then bool "un-" "" publish ++ "published: " ++ show meta
            else "run not in DB"

storeRunsToDB :: DBSchema -> DB.Connection -> Either [FilePath] (ClusterRun AsValue) -> ExceptT String IO ()
storeRunsToDB dbSchema conn runs
  = do
    anyCreated <- foldM go False (either (map Left) (\r -> [Right r]) runs)

    -- for any change to the run list itself (not the associated results)
    -- we need to refresh the materialized view
    when anyCreated $
      void $ liftDBRun (dbRefreshView dbSchema) conn
  where
    getClusterRun :: Either FilePath (ClusterRun AsValue) -> ExceptT String IO (ClusterRun ByteString)
    getClusterRun (Left metaFile) = liftIO (loadClusterRun metaFile) >>= hoistEither >>= getClusterRun . Right
    getClusterRun (Right cr)      = pure $ valueToBS `fmap` cr

    logClusterRun created (Left metaFile)
      = liftIO $  putStrLn $ bool "update" "create" created ++ ": " ++ metaFile
    logClusterRun _ _
      = pure ()

    go :: Bool -> Either FilePath (ClusterRun AsValue) -> ExceptT String IO Bool
    go !created run_
      = do
        run <- getClusterRun run_
        created' <- liftDBRun (dbStoreRun dbSchema run) conn
        logClusterRun created' run_
        pure $ created' || created

searchRuns :: FilePath -> IO [FilePath]
searchRuns targetDir
  = do
    subDirs <- listDirectories targetDir
    filterM doesFileExist $ map (</> "meta.json") subDirs

normalizeMetaFilePath :: FilePath -> FilePath
normalizeMetaFilePath metaFile
  | takeFileName metaFile == "meta.json" = metaFile
  | otherwise = metaFile </> "meta.json"

-- given a path to its directory or meta.json, loads all pertaining data into a ClusterRun
loadClusterRun :: FilePath -> IO (Either String (ClusterRun AsValue))
loadClusterRun metaFile_
  = do
    runMeta_ <- BS.readFile metaFile
    case eitherDecodeStrict' runMeta_ of
        Left e -> pure $ Left e
        Right metaStub -> do
          let runMeta = fromJust $ decodeStrict runMeta_
          runBlockProp <- tryReadFile $ analysis </> "blockprop.json"
          runClusterPerf <- tryReadFile $ analysis </> "clusterperf.json"
          pure $! Right $! ClusterRun{..}
  `catch`
    \(SomeException e) -> pure (Left $ show e)
  where
    metaFile = normalizeMetaFilePath metaFile_
    analysis = takeDirectory metaFile </> "analysis"
    tryReadFile f
      = doesFileExist f >>= bool (pure Nothing) (decodeFileStrict f)

getDBSettings :: DBCredentials -> IO DB.Settings
getDBSettings NoCreds
  = maybe BS.empty BS.pack <$> lookupEnv envVarDBURI
getDBSettings (PostgresURI uri)
  = pure $ BS.pack uri
getDBSettings DBCreds{..}
  = do
    envUser <- BS.pack <$> getLoginName
    envPass <- maybe BS.empty BS.pack <$> lookupEnv envVarDBPass
    pure $ DB.settings
      (maybe "localhost" BS.pack dbHost)
      (maybe 5432 fromIntegral dbPort)
      (maybe envUser BS.pack dbUser)
      (maybe envPass BS.pack dbPass)
      (BS.pack dbName)

getBuiltInSql :: FilePath -> ExceptT String IO SqlSource
getBuiltInSql
  = handleIOExceptT show . loadBuiltinSql

loadBuiltinSql :: FilePath -> IO SqlSource
loadBuiltinSql sqlName
  = do
    fn <- getDataFileName ("db" </> sqlName)
    SqlSource <$> BS.readFile fn
