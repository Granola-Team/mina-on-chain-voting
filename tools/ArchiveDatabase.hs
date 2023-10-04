{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  (main) where

import Control.Monad (forever, guard, void, when)
import Control.Monad.Catch (bracket)
import Data.ByteString (hGetContents)
import Data.List (sort)
import Data.Map.Strict (fromList)
import Data.Monoid (Last (..))
import Data.String (IsString (fromString))
import Data.Text.Short (toString)
import Database.PostgreSQL.Simple (close, connectPostgreSQL, execute_)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Options (Options (Options, dbname, host, user), defaultOptions)
import Database.PostgreSQL.Simple.Types (Query (..))
import Database.Postgres.Temp (CommandLineArgs (keyBased), Config (connectionOptions, dataDirectory, initDbConfig, logger, port, postgresConfig, postgresConfigFile), DirectoryType (Permanent), Event, ProcessConfig (ProcessConfig, commandLine), startConfig, toConnectionString, with, withConfig)
import System.Directory
    ( getDirectoryContents, removeDirectoryRecursive, removeDirectory )
import System.IO (IOMode (ReadMode), withFile)
import Lib.ArchiveDump
    ( ArchiveDump(ArchiveDump, dumpMetadata), parseDumps, MinaNetwork, ArchiveDumpMetadata (dumpNetwork), parseArchiveDumpMetadata )
import Lib.DatabaseCommands (restoreDatabaseBackup)
import System.Environment (getEnv)
import Data.Kind (Type)
import Data.Int (Int64)
import Control.Monad.Cont (MonadIO (liftIO))
import Data.Functor ((<&>))
import Options.Applicative
import System.Exit
import System.FilePath (takeBaseName)

data ArchiveDatabaseCli = Cli
  { archiveDump        :: FilePath
  , instanceDir        :: FilePath
  , dbUser             :: String
  , removeFilesOnClose :: Bool
  }

archiveDatabaseCli :: Parser ArchiveDatabaseCli
archiveDatabaseCli = Cli
  <$> strOption
    ( long "archive-dump"
    <> metavar "PATH" )
  <*> strOption
    ( long "instance-dir"
    <> showDefault
    <> value ".pg"
    <> metavar "PATH" )
  <*> strOption
     ( long "user"
    <> help "which user to set up the database for"
    <> showDefault
    <> value "$USER"
    <> metavar "USER" )
  <*> flag True False
     ( long "keep-files"
    <> short 'k'
    <> help "Keep database instance files after shutdown" )

databaseDumpDir = "database_dumps"

archivePostgresConfig =
  [ ("log_min_messages", "warning"),
    ("log_min_error_statement", "error"),
    ("log_min_duration_statement", "100  # ms"),
    ("log_connections", "on"),
    ("log_disconnections", "on"),
    ("log_duration", "on"),
    ("#log_line_prefix", "'[] '"),
    ("log_timezone", "'UTC'"),
    ("log_statement", "'all'"),
    ("log_directory", "'pg_log'"),
    ("log_filename", "'postgresql-%Y-%m-%d_%H%M%S.log'"),
    ("logging_collector", "on"),
    ("log_min_error_statement", "error")
  ]

archiveInitDBConfig = pure $
  mempty
    { commandLine =
        mempty
          { keyBased =
              fromList
                [ ("--no-locale", Nothing),
                  ("--encoding=", Just "UTF8")
                ]
          }
    }

archiveDatabaseConfig user = pure $ mempty
  { logger = mempty, -- pure databaseLogger
    postgresConfigFile = archivePostgresConfig,
    initDbConfig = archiveInitDBConfig,
      
    -- , postgresConfig = mempty
    --     { commandLine = mempty
    --         { keyBased = fromList
    --         [ ("--no-locale", Nothing)
    --         , ("--encoding=", Just "UTF8")
    --         ]
    --         }
    --     }
    dataDirectory = Permanent ".pg",
    port = Last (Just $ Just 5432),
    connectionOptions =
      mempty
        { host = pure "localhost",
          user = pure user
        }
  }

databaseLogger :: Event -> IO ()
databaseLogger = print

envVarDatabaseConfig :: IO Config
envVarDatabaseConfig = do
  user <- getEnv "USER"
  archiveDatabaseConfig user

cliDatabaseConfig :: IO (ArchiveDatabaseCli, Config)
cliDatabaseConfig = do
  cli <- execParser opts
  config <- archiveDatabaseConfig (dbUser cli)
  pure (cli, config)
    where
      opts = info (archiveDatabaseCli <**> helper)
        ( fullDesc
        <> progDesc "Run an archive database from a SQL dump" )
    

migrateArchiveDumpBackup :: FilePath -> PG.Connection -> IO ()
migrateArchiveDumpBackup dumpPath conn = do
  putStrLn "Initializing a temporary archive database!"
  case parseArchiveDumpMetadata (takeBaseName dumpPath) of
    Nothing -> putStrLn ("Unable to parse archive dump: " ++ dumpPath) 
      >> exitFailure
    Just (ArchiveDump targetKey metadata) -> do
      execute_ conn "CREATE DATABASE archive;"
      execute_ conn "CREATE DATABASE archive_balances_migrated"
      restoreDatabaseBackup dumpPath

withConnection :: ArchiveDatabaseCli -> PG.Connection -> IO ()
withConnection cli conn = do
  putStrLn "creating archive and archive_balances_migrated databases..."
  migrateArchiveDumpBackup (archiveDump cli) conn
  putStrLn "entering idle..."
  databaseLoop

databaseLoop :: IO ()
databaseLoop = do
  databaseLoop

shutdownDatabase :: ArchiveDatabaseCli -> PG.Connection -> IO ()
shutdownDatabase cli conn = do
  putStrLn "closing databse connection..."
  close conn
  when (removeFilesOnClose cli) $ do
    putStrLn "removing database files..."
    removeDirectoryRecursive ".pg"

main :: IO ()
main = do
  putStrLn "Initializing Database Config"
  (cli, config) <- cliDatabaseConfig
  putStrLn "Got Database Config"
  res <- withConfig config $ \db ->
    bracket
      (connectPostgreSQL (toConnectionString db))
      (shutdownDatabase cli)
      (withConnection cli)
  case res of
    Left r -> print r
    Right _ -> pure ()
