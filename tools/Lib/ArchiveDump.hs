module Lib.ArchiveDump where
import Text.Megaparsec (Parsec, MonadParsec (try), (<|>), many, runParser)
import Data.Void (Void)
import Text.Megaparsec.Char (string, digitChar, char)
import Data.Maybe (mapMaybe)
import qualified Text.ParserCombinators.ReadPrec

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
  dumpMetadata <- case runParser dumpParser "" dumpName of
    Left _ -> Nothing
    Right metadata -> Just metadata
  pure $ ArchiveDump dumpName dumpMetadata

parseDumps :: [String] -> [ArchiveDump]
parseDumps = mapMaybe parseArchiveDumpMetadata

data MinaNetwork
  = Mainnet
  | Devnet
  | Berkeley
  deriving (Eq, Ord, Enum, Read)
instance Show MinaNetwork where
  show :: MinaNetwork -> String
  show Mainnet = "mainnet"
  show Devnet = "devnet"
  show Berkeley = "berkeley"

data ArchiveDumpMetadata = 
  ArchiveDumpMetadata
  { dumpNetwork :: MinaNetwork
  , year :: Int
  ,  month :: Int
  , day :: Int
  } deriving (Show, Eq)

instance Ord ArchiveDumpMetadata where
  compare d1 d2
    | year d1 /= year d2 = compare (year d1) (year d2)
    | month d1 /= month d2 = compare (month d1) (month d2)
    | otherwise = compare (day d1) (day d2)

minaNetwork :: Parser MinaNetwork
minaNetwork =
  try (Devnet <$ string "devnet-archive-dump-") 
  <|> (Mainnet <$ string "mainnet-archive-dump-")
  <|> (Mainnet <$ string "archive-dump-")
  <|> (Berkeley <$ string "berkeley-archive-dump-")

dumpDate :: Parser (Int, Int, Int)
dumpDate = do
  year <- many digitChar
  char '-'
  month <- many digitChar
  char '-'
  day <- many digitChar
  pure (read year, read month, read day)

dumpParser :: Parser ArchiveDumpMetadata
dumpParser = do
  network <- minaNetwork
  (year, month, day) <- dumpDate
  pure $ ArchiveDumpMetadata network year month day