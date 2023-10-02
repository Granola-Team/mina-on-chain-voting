{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Fetchers where
import Text.XML.Light (Content, parseXML)
import Network.Curl (CurlResponse_ (respBody), withCurlDo, curlGetResponse_, URLString, CurlOption)
import Data.ByteString ( ByteString )

databaseDumpIndexURL :: URLString
databaseDumpIndexURL = "https://storage.googleapis.com/mina-archive-dumps/"

curlOptions :: [CurlOption]
curlOptions = []


fetchDatabaseDumpIndex :: IO [Content]
fetchDatabaseDumpIndex = withCurlDo $ do
  response :: CurlResponse_ [(String, String)] String 
    <- curlGetResponse_ databaseDumpIndexURL curlOptions

  return . parseXML . respBody $ response

fetchArchiveDump :: String -> IO ByteString
fetchArchiveDump key = withCurlDo $ do
  response :: CurlResponse_ [(String, String)] ByteString 
    <- curlGetResponse_ (databaseDumpIndexURL ++ key) curlOptions

  return (respBody response)