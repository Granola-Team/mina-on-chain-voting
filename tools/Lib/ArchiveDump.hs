module Lib.ArchiveDump where
import Text.Megaparsec (Parsec, MonadParsec (try), (<|>), many, runParser)
import Data.Void (Void)
import Text.Megaparsec.Char (string, digitChar, char)
import Data.Maybe (mapMaybe)

type Parser = Parsec Void String

data ArchiveDump = ArchiveDump
  { dumpName :: String
  , dumpMetadata :: ArchiveDumpMetadata
  } deriving (Show, Eq)
instance Ord ArchiveDump where
  compare (ArchiveDump _ meta1) (ArchiveDump _ meta2) =
    meta1 `compare` meta2

parseArchiveDumpMetadata :: String -> Maybe ArchiveDump
parseArchiveDumpMetadata dumpName = do
  dumpMetadata <- case runParser devnetParser "" dumpName of
    Left _ -> Nothing
    Right metadata -> Just metadata
  pure $ ArchiveDump dumpName dumpMetadata

associateKeyMetadata :: [String] -> [ArchiveDump]
associateKeyMetadata = mapMaybe parseArchiveDumpMetadata

data ArchiveDumpMetadata = 
  ArchiveDumpMetadata
  { version :: Int,
    year :: Int,
    month :: Int,
    day :: Int
  }
  deriving (Show, Eq)

instance Ord ArchiveDumpMetadata where
  compare d1 d2
    | year d1 /= year d2 = compare (year d1) (year d2)
    | month d1 /= month d2 = compare (month d1) (month d2)
    | otherwise = compare (day d1) (day d2)


devnetVersion :: Parser Int
devnetVersion =
  try (1 <$ string "devnet-archive-dump-")
    <|> (2 <$ string "devnet2-archive-dump-")

devnetDate :: Parser (Int, Int, Int)
devnetDate = do
  year <- many digitChar
  char '-'
  month <- many digitChar
  char '-'
  day <- many digitChar
  pure (read year, read month, read day)

devnetParser :: Parser ArchiveDumpMetadata
devnetParser = do
  version <- try devnetVersion
  (year, month, day) <- devnetDate
  pure $ ArchiveDumpMetadata version year month day