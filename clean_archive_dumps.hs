module Main where 

import System.Directory
import Lib.ArchiveDump (associateKeyMetadata, ArchiveDump (ArchiveDump, dumpName))
import Data.List (sort)
import Control.Monad (unless)

databaseDumpDir = "database_dumps"

main :: IO ()
main = do
    setCurrentDirectory databaseDumpDir
    filenames <- getCurrentDirectory >>= getDirectoryContents
    let archiveDumps = reverse . sort $ associateKeyMetadata filenames
    let (ArchiveDump targetKey metadata):dumps = archiveDumps
    putStrLn $ "The latest archive dump is " ++ targetKey
    putStr "Would you like to delete old archive dumps? (y/N) "
    resp <- getLine

    unless (resp == "y") $ mapM_ (removeFile . dumpName) dumps