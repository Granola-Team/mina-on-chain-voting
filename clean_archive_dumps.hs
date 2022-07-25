module Main where

import Control.Monad (when)
import Data.List (sort)
import Lib.ArchiveDump (ArchiveDump (ArchiveDump, dumpName), associateKeyMetadata)
import System.Directory
    ( getCurrentDirectory,
      getDirectoryContents,
      removeFile,
      setCurrentDirectory )

databaseDumpDir = "database_dumps"

main :: IO ()
main = do
  setCurrentDirectory databaseDumpDir
  filenames <- getCurrentDirectory >>= getDirectoryContents
  let archiveDumps = reverse . sort $ associateKeyMetadata filenames
  let (ArchiveDump targetKey metadata) : dumps = archiveDumps
  putStrLn $ "The latest archive dump is " ++ targetKey
  putStr "Would you like to delete old archive dumps? (y/N) "
  resp <- getLine

  when (resp == "y") $ mapM_ (removeFile . dumpName) dumps