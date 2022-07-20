module Lib.ArchiveDumpKeyParser where
import Text.Megaparsec (Parsec, MonadParsec (try), (<|>), many)
import Data.Void (Void)
import Text.Megaparsec.Char (string, digitChar, char)

type Parser = Parsec Void String

data ArchiveDumpKey = ArchiveDumpKey
  { version :: Int,
    year :: Int,
    month :: Int,
    day :: Int
  }
  deriving (Show, Eq)

instance Ord ArchiveDumpKey where
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

devnetParser :: Parser ArchiveDumpKey
devnetParser = do
  version <- try devnetVersion
  (year, month, day) <- devnetDate
  pure $ ArchiveDumpKey version year month day