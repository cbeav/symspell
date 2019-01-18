-- |
--
-- 'IO' operations for constructing a 'SymSpell' from a 'FilePath'.
--
module SymSpell.IO (fromFile) where

import ClassyPrelude hiding (fromList, readFile)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Text.IO (readFile)
import SymSpell
import Prelude (read)

-- | Read a word and its frequency count separated by a space.
parseFrequency :: Parser (Text, Int)
parseFrequency =
  (,)
    <$> (parseTextUntil " "     <* " ")
    <*> ((read <$> many1 digit) <* (endOfLine <|> endOfInput))
 where
  parseTextUntil = map pack . manyTill anyChar . lookAhead

-- | Parse many word count tuples from a given file and construct
-- a 'SymSpell'.
fromFile :: SymSpellConfig -> FilePath -> IO SymSpell
fromFile config file = do
  Right freqs <- parseOnly (many parseFrequency) <$> readFile file
  pure $ fromList config freqs
