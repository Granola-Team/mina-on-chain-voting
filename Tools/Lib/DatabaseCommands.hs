module Lib.DatabaseCommands where
import System.Process (createProcess, CreateProcess (CreateProcess), callCommand)

restoreDatabaseBackup :: String -> IO ()
restoreDatabaseBackup archiveKey = do
    callCommand $ "psql -h localhost -p 5432 -U $USER -d postgres -f " ++ archiveKey