{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Archive.Tar (extract)
import Codec.Compression.GZip (decompress)
import Control.Monad (unless, when, void, forever)
import Control.Monad.Catch (ExitCase (ExitCaseAbort), bracket)
import Data.ByteString (ByteString, writeFile, hPut)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Data (DataRep)
import Data.Either (rights)
import Data.Functor ((<&>))
import Data.List ( isInfixOf, sortBy, sort )
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Void (Void)
import Distribution.Compat.CharParsing (digit)
import Network.Curl (CurlOption, CurlResponse_ (respBody), URLString, curlGetResponse_, withCurlDo)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (ExitSuccess), exitSuccess, exitWith)
import System.IO (IOMode (WriteMode), hClose, openBinaryFile, withBinaryFile, withFile, stdout, hPutStrLn)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    many,
    runParser,
    (<|>),
  )
import Text.Megaparsec.Char (char, digitChar, string)
import Text.XML.Light (Attr, CData (cdData), Content (Elem, Text), Element (Element, elAttribs, elContent, elName), QName (qName), parseXML)

import Lib.ArchiveDump
    ( ArchiveDump(ArchiveDump), associateKeyMetadata )
import Lib.Fetchers (fetchDatabaseDumpIndex, fetchArchiveDump)
import qualified Data.ByteString as Prelude
import Database.Postgres.Temp (Config(..), defaultConfig, DirectoryType (Permanent), with, toConnectionString)
import Data.Monoid (Last(..))
import Database.PostgreSQL.Simple (connectPostgreSQL, close, execute_)

getListBucketsResult :: Content -> [Content]
getListBucketsResult = \case
  Elem (Element name attrs content line) ->
    if qName name == "ListBucketResult"
      then drop 4 content
      else []
  _ -> []

getDevnetKeys :: [Content] -> [String]
getDevnetKeys =
  mapMaybe
    ( \content -> do
        keyElement <- case content of
          Elem (Element name attrs content line) ->
            if qName name == "Contents"
              then Just $ head content
              else Nothing

        textElement <- case keyElement of
          Elem (Element name attrs content line) ->
            if qName name == "Key"
              then Just $ head content
              else Nothing
          _ -> Nothing

        case textElement of
          Text cd ->
            let dumpName = cdData cd
             in if "devnet" `isInfixOf` dumpName
                  then Just dumpName
                  else Nothing
          _ -> Nothing
    )

main :: IO ()
main = do
  putStrLn "getting database backup keys..."
  devnetKeys <- fetchDatabaseDumpIndex <&> (getDevnetKeys . getListBucketsResult . (!! 1))
  let keysByDate = sort $ associateKeyMetadata devnetKeys
  let (ArchiveDump targetKey metadata) = last keysByDate
  let archiveDumpTar = "database_dumps/" ++ targetKey
  let archiveDumpFilename = take (length archiveDumpTar - length (".tar.gz" :: String)) archiveDumpTar

  archiveDumpExists <- doesFileExist archiveDumpFilename
  when archiveDumpExists $ do
    putStr
      ( "archive dump \""
          ++ archiveDumpFilename
          ++ "\" exists, would you like to overwrite? (y/N): "
      )
    resp <- getLine
    unless (resp == "y") exitSuccess

  putStrLn $ "donwloading archive dump " ++ targetKey ++ "..."
  archiveDumpCompressed <- fetchArchiveDump targetKey
  let archiveDump = decompress . fromStrict $ archiveDumpCompressed

  putStrLn "writing archive file..."
  withBinaryFile archiveDumpTar WriteMode
    ( \handle -> do hPut handle (toStrict archiveDump) )

  putStrLn "extracting archive dump..."
  extract "database_dumps/" archiveDumpTar

  removeFile archiveDumpTar
  