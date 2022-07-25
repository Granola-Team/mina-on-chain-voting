{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever, guard, void, when)
import Control.Monad.Catch (bracket)
import Data.List (sort)
import Data.Map.Strict (fromList)
import Data.Monoid (Last (..))
import Data.String (IsString (fromString))
import Data.Text.Short (toString)
import Database.PostgreSQL.Simple (close, connectPostgreSQL, execute_)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Options (Options (Options, dbname, host, user), defaultOptions)
import Database.Postgres.Temp (CommandLineArgs (keyBased), Config (connectionOptions, dataDirectory, initDbConfig, logger, port, postgresConfig, postgresConfigFile), DirectoryType (Permanent), Event, ProcessConfig (ProcessConfig, commandLine), startConfig, toConnectionString, with, withConfig)
import Tools.Lib.ArchiveDump
import Tools.Lib.DatabaseCommands (restoreDatabaseBackup)
import System.Directory
import System.IO (IOMode (ReadMode), withFile)
import Data.ByteString (hGetContents)
import Database.PostgreSQL.Simple.Types (Query(..))

databaseLogger :: Event -> IO ()
databaseLogger = print

databaseConfig :: Config
databaseConfig =
  mempty
    { logger = mempty, -- pure databaseLogger
      postgresConfigFile =
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
        ],
      initDbConfig =
        pure $
          mempty
            { commandLine =
                mempty
                  { keyBased =
                      fromList
                        [ ("--no-locale", Nothing),
                          ("--encoding=", Just "UTF8")
                        ]
                  }
            },
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
            user = pure "jenr"
          }
    }

databaseDumpDir = "database_dumps"

migrateArchiveDumpBackup :: PG.Connection -> IO ()
migrateArchiveDumpBackup conn = do
  filenames <- getDirectoryContents databaseDumpDir
  let archiveDumps = reverse . sort $ associateKeyMetadata filenames
  let (ArchiveDump targetKey metadata) : dumps = archiveDumps

  let archiveDumpFilename = databaseDumpDir ++ "/" ++ targetKey
  execute_ conn "CREATE DATABASE archive;"
  execute_ conn "CREATE DATABASE archive_balances_migrated"
  
--   putStr "would you like to restore the latest archive backup? (y/N) "
--   restore <- getLine
--   when (restore == "y") $ 
  restoreDatabaseBackup archiveDumpFilename

withConnection :: PG.Connection -> IO ()
withConnection conn = do
  putStrLn "creating archive and archive_balances_migrated databases..."
  migrateArchiveDumpBackup conn
  putStrLn "entering idle..."
  databaseLoop

databaseLoop :: IO ()
databaseLoop = do
  databaseLoop

shutdownDatabase :: PG.Connection -> IO ()
shutdownDatabase conn = do
  putStrLn "closing databse connection..."
  close conn
  putStrLn "removing temporary database files..."
  removeDirectoryRecursive ".pg"

main :: IO ()
main = do
  res <- withConfig databaseConfig $ \db ->
    bracket
      (connectPostgreSQL (toConnectionString db))
      shutdownDatabase
      withConnection
  case res of
    Left r -> print r
    Right _ -> pure ()