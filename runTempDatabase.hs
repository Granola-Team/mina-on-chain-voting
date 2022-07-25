module Main where
import Database.Postgres.Temp (Config (logger, postgresConfigFile, initDbConfig), with, toConnectionString, ProcessConfig (commandLine, ProcessConfig), CommandLineArgs (keyBased), withConfig)
import Lib.ArchiveDump
import Data.List (sort)
import System.Directory
import System.IO (withBinaryFile, IOMode (ReadMode), withFile)
import Database.PostgreSQL.Simple (execute_, connectPostgreSQL, close)
import qualified Database.PostgreSQL.Simple as PG
import Data.String ( IsString(fromString) )
import Control.Monad.Catch (bracket)
import Data.Map.Strict (fromList)

databaseConfig :: Config
databaseConfig = mempty
    { logger = pure mempty
    , postgresConfigFile = pure mempty
    , initDbConfig = pure $ mempty
        { commandLine = mempty 
            { keyBased = fromList 
            [ ("--no-locale", Nothing)
            , ("--encoding=", Just "UTF8")
            , ("-D ", Just ".pg")
            ] 
            }
        }
    }

databaseDumpDir = "database_dumps"

migrateArchiveDumpBackup :: PG.Connection -> IO ()
migrateArchiveDumpBackup conn = do
    setCurrentDirectory databaseDumpDir
    filenames <- getCurrentDirectory >>= getDirectoryContents
    let archiveDumps = reverse . sort $ associateKeyMetadata filenames
    let (ArchiveDump targetKey metadata) : dumps = archiveDumps

    dumpText <- readFile targetKey

    execute_ conn $ fromString dumpText

    pure ()

main :: IO ()
main = do
    res <- withConfig databaseConfig $ \db -> bracket
        (connectPostgreSQL (toConnectionString db))
        close migrateArchiveDumpBackup

    case res of
        Left r -> print r
        Right _ -> pure ()