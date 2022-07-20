{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Network.Curl (withCurlDo, CurlOption, curlGetResponse_, URLString, CurlResponse_ (respBody))
import Text.XML.Light (Content (Elem, Text), parseXML, Element(Element, elName, elAttribs, elContent), QName (qName), CData (cdData), Attr)
import Data.List (isInfixOf, sortBy)
import Data.Functor ((<&>))
import Data.Maybe (isJust, catMaybes, mapMaybe)
import Data.Time (UTCTime)
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char (string, digitChar, char)
import Data.Data (DataRep)
import Distribution.Compat.CharParsing (digit)
import Data.Either (rights)
import Data.ByteString (ByteString, writeFile, hPut)
import System.IO (withFile, IOMode (WriteMode), openBinaryFile, hClose)

databaseDumpIndexURL :: URLString
databaseDumpIndexURL = "https://storage.googleapis.com/mina-archive-dumps/"

curlOptions :: [CurlOption]
curlOptions = []

getDatabaseDumpIndex :: IO [Content]
getDatabaseDumpIndex = withCurlDo $ do
    response :: CurlResponse_ [(String, String)] String
        <- curlGetResponse_ databaseDumpIndexURL curlOptions

    return . parseXML . respBody $ response

getListBucketsResult :: Content -> [Content]
getListBucketsResult = \case
    Elem (Element name attrs content line) ->
        if qName name == "ListBucketResult"
            then drop 4 content
            else []
    _ -> []

getDevnetKeys :: [Content] ->  [String]
getDevnetKeys = mapMaybe (\case
        Elem (Element name attrs content line) -> if qName name == "Contents"
            then case head content of
                Elem (Element name attrs content line) -> if qName name == "Key"
                    then case head content of
                        Text cd -> let dumpName = cdData cd
                            in if "devnet" `isInfixOf` dumpName
                                then Just dumpName
                                else Nothing
                        _ -> Nothing
                    else Nothing
            else Nothing
        _ -> Nothing
    )

type Parser = Parsec Void String

data DevnetDump = DevnetDump
    { version :: Int
    , year :: Int
    , month :: Int
    , day :: Int
    }
    deriving (Show, Eq)

instance Ord DevnetDump where
  compare d1 d2
    | year d1 /= year d2 = compare (year d1) (year d2)
    | month d1 /= month d2 = compare (month d1) (month d2)
    | otherwise = compare (day d1) (day d2)

devnetVersion :: Parser Int
devnetVersion =
    try (1 <$ string "devnet-archive-dump-") <|>
    (2 <$ string "devnet2-archive-dump-")

devnetDate :: Parser (Int, Int, Int)
devnetDate = do
    year <- many digitChar
    char '-'
    month <- many digitChar
    char '-'
    day <- many digitChar
    pure (read year, read month, read day)

devnetParser :: Parser DevnetDump
devnetParser = do
    version <- try devnetVersion
    (year, month, day) <- devnetDate
    pure $ DevnetDump version year month day


getUTCfromKeys :: [String] -> [Maybe UTCTime]
getUTCfromKeys = undefined

associateKeyMetadata :: [String] -> [(String, DevnetDump)]
associateKeyMetadata keys =
    let metadata = rights . map (runParser devnetParser "") $ keys
    in zip keys metadata

getArchiveDump :: String -> IO ByteString
getArchiveDump key = withCurlDo $ do
    response :: CurlResponse_ [(String, String)] ByteString
        <- curlGetResponse_ (databaseDumpIndexURL ++ "/" ++ key) curlOptions

    return (respBody response)

main :: IO ()
main = do
    devnetKeys <- getDatabaseDumpIndex <&> (getDevnetKeys . getListBucketsResult . (!!1))
    let keysByDate = sortBy (\(_, x)  (_, y) -> x `compare` y) $ associateKeyMetadata devnetKeys
    let (targetKey, _)= last keysByDate
    dump_bytes <- getArchiveDump targetKey

    print dump_bytes
    -- handle <- openBinaryFile ("database_dumps/" ++ targetKey) WriteMode
    -- hPut handle dump_bytes
    -- hClose handle