{-# LANGUAGE OverloadedStrings #-}
import Options.Applicative
import Data.ByteString.Char8 (pack)
import Lib.ArchiveDump (ArchiveDump(..), parseArchiveDumpMetadata)
import System.FilePath
import System.Exit (exitFailure)
import Lib.DatabaseCommands (restoreDatabaseBackup)
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.Catch (bracket)
data ArchiveDatabaseCli = Cli
  { archiveDump        :: FilePath
  , databaseUrl        :: String
  }

archiveRestoreCli :: Parser ArchiveDatabaseCli
archiveRestoreCli = Cli
  <$> strOption
    ( long "archive-dump"
    <> metavar "PATH" )
  <*> strOption
     ( long "database-url"
    <> help "database url to connect to PSQL"
    <> showDefault
    <> value "$USER"
    <> metavar "USER" )

opts = info (archiveRestoreCli <**> helper)
     ( fullDesc
    <> progDesc "Restore an archive dump to a running PostgreSQL database" )

main :: IO ()
main = do
    cli <- execParser opts
    putStrLn "Restoring Database..."
    conn :: PG.Connection <- (connectPostgreSQL. pack . databaseUrl) cli
    bracket
        (putStrLn ("Connecting to " ++ databaseUrl cli) 
            >> (connectPostgreSQL. pack . databaseUrl) cli)
        (\conn -> putStrLn "Shutting Down DB Connection" 
            >> close conn)
        (\conn -> putStrLn "Migrating Archive Dump" 
            >> migrateArchiveDumpBackup (archiveDump cli) conn )

migrateArchiveDumpBackup :: FilePath -> PG.Connection -> IO ()
migrateArchiveDumpBackup dumpPath conn = do
  putStrLn "Initializing a temporary archive database!"
  case parseArchiveDumpMetadata (takeBaseName dumpPath) of
    Nothing -> putStrLn ("Unable to parse archive dump: " ++ dumpPath) 
      >> exitFailure
    Just (ArchiveDump targetKey metadata) -> do
      PG.execute_ conn "CREATE DATABASE archive;"
      PG.execute_ conn "CREATE DATABASE archive_balances_migrated"
      restoreDatabaseBackup dumpPath