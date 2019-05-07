{-# LANGUAGE DataKinds #-}
-- |
--
-- A Haskell implementation of Wolf Garbe's
-- <https://github.com/wolfgarbe/SymSpell SymSpell> algorithm.
--
module SymSpell
  ( fromFile
  , fromFileContents
  , fromList
  , suggest
  , SymSpell(..)
  , SymSpellConfig(..)
  , SymSpellSuggestion(..)
  , SymSpellVerbosity(..)
  ) where

import ClassyPrelude hiding (fromList, head, readFile)
import Prelude (head, read)

import Closed
import Control.Arrow ((&&&))
import Data.Aeson
import Data.Aeson.Casing
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Bits
import Data.Char (ord)
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.IO (readFile)
import Data.Text.Metrics (damerauLevenshtein)
import GHC.Compact

-- | Verbosity for determining suggestions.
--
-- 'Top' returns the most common, lowest distance relative.
-- 'Closest` returns all suggestions of lowest matched distance.
-- 'All' provides all suggestions within the given distance cap.
data SymSpellVerbosity
  = Top
  | Closest
  | All
  deriving (Eq, Show)

-- | Deletions are hashed with intentional collision to cut down
-- on required memory. A compaction level of zero will take the most
-- memory, but provide the fastest algorithm. Sixteen will minimize
-- memory usage in exchange for slower queries.
type CompactionLevel = Closed 0 16

-- | SymSpell precomputes all deletions of the input dictionary within
-- a provided maximum distance. We restrict our precomputation and search
-- space to a fixed prefix length. For example, if our dictionary contains
-- the word "example", and our prefix length is four, and our edit distance
-- is one, we would store pointers for "exam", "exa", "exm", "eam", and
-- "xam" back to the known word. In considering the input "exampel", we
-- look at its deletions, find matches in the map, and evaluate edit
-- distance there to find valid suggestions.
data SymSpellConfig
  = SymSpellConfig
  { symSpellConfigMaxEditDistance :: !Int -- ^ Number of deletions to precompute.
  , symSpellConfigPrefixLength    :: !Int -- ^ Length cap on search space.
  , symSpellConfigCompactionLevel :: !CompactionLevel -- ^ Compaction level used for forcing hash conflicts.
  } deriving (Eq, Show)

instance Default SymSpellConfig where
  def = SymSpellConfig 2 7 5

-- | Function for cutting down our search space.
type PrefixFunction = Text -> Text

-- | Function to allow for compact hashing.
type HashFunction = Text -> Word32

data SymSpell
  = SymSpell
  { symSpellPrefix      :: !PrefixFunction -- ^ Function to restrict search space.
  , symSpellHash        :: !HashFunction -- ^ Function for compressed hashing.
  , symSpellDeletes     :: !(Compact (HashMap Word32 (Set Text))) -- ^ Pre-computed deletions.
  , symSpellFrequencies :: !(Compact (HashMap Text Int)) -- ^ Dictionary with frequency counts.
  }

-- | Suggestions reference a dictionary word and an edit distance from
-- the provided target.
data SymSpellSuggestion
  = SymSpellSuggestion
  { symSpellSuggestionWord     :: !Text
  , symSpellSuggestionDistance :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON SymSpellSuggestion where
  toJSON = genericToJSON $ aesonDrop (length ("symSpellSuggestion" :: Text)) camelCase

-- | Read a word and its frequency count separated by a space.
parseFrequency :: Parser (Text, Int)
parseFrequency =
  (,)
    <$> (parseTextUntil " "     <* " ")
    <*> ((read <$> many1 digit) <* (endOfLine <|> endOfInput))
 where
  parseTextUntil = map pack . manyTill anyChar . lookAhead

-- | Read a frequency file to construct a 'SymSpell'.
fromFile :: SymSpellConfig -> FilePath -> IO SymSpell
fromFile config file = fromFileContents config =<< readFile file

-- | Parse many word count tuples from a given newline-delimited,
-- space separated frequency list and construct a 'SymSpell'.
fromFileContents :: SymSpellConfig -> Text -> IO SymSpell
fromFileContents config contents =
  let Right freqs = parseOnly (many parseFrequency) contents
  in  fromList config freqs

-- | Construct a 'SymSpell' from a 'SymSpellConfig' and frequency tuples.
fromList :: SymSpellConfig -> [(Text, Int)] -> IO SymSpell
fromList SymSpellConfig{..} items = do
  let
    symSpellPrefix = take symSpellConfigPrefixLength
    symSpellHash = compactHash symSpellConfigCompactionLevel
    words = map fst items
    deleteHashes = toList . Set.map symSpellHash . deletesPrefix symSpellPrefix symSpellConfigMaxEditDistance
    deleteMap w = HashMap.fromList . map (,Set.singleton w) $ deleteHashes w
  symSpellFrequencies <- compact $ HashMap.fromList items
  symSpellDeletes     <- compact $ foldr (HashMap.unionWith Set.union . deleteMap) HashMap.empty words
  pure SymSpell{..}

-- | Generate a list of 'SymSpellSuggestion' for a given 'SymSpell'.
suggest :: SymSpell -> Int -> SymSpellVerbosity -> Text -> [SymSpellSuggestion]
suggest SymSpell{..} maxDistance verbosity input =
  let
    searchSpace = candidatesWithinDistance maxDistance $ symSpellPrefix input
    matchesFor candidate = HashMap.lookupDefault Set.empty (symSpellHash candidate) $ getCompact symSpellDeletes
    matches = toList . Set.unions $ map matchesFor searchSpace
    distances = map (id &&& damerauLevenshtein input) matches
    results = map (uncurry SymSpellSuggestion) $ sortOn snd $ filter ((<= maxDistance) . snd) distances
  in
    case (results, verbosity) of
      ([], _) -> []
      (top:_, Closest) -> takeWhile ((== symSpellSuggestionDistance top) . symSpellSuggestionDistance) results
      (top:_, Top) -> [top]
      _ -> results

-- | Build a 'HashFunction' for a fixed 'CompactionLevel'.
compactHash :: CompactionLevel -> HashFunction
compactHash level str =
  let
    mask = shiftL (shiftR (maxBound :: Word32) (3 + fromInteger (getClosed level))) 2
    mixChar c = (*16777619) . (`xor` fromIntegral (ord c))
    lenMask = min 3 . fromIntegral $ length str
  in
    (.|. lenMask) . (.&. mask) $ foldr mixChar 2166136261 str

-- | Generate and dedup all deletions within given distance.
deletesPrefix :: PrefixFunction -> Int -> Text -> Set Text
deletesPrefix prefix distance = deletesWithinDistance distance . prefix

-- | Generate all deletions of unit distance from a given word.
deletes :: Text -> Set Text
deletes word =
  Set.fromList $ word : map (withoutN word) [0..(length word - 1)]
 where
  withoutN str n = take n str ++ drop (n + 1) str

-- | Recursively construct all deletions within given distance.
deletesWithinDistance :: Int -> Text -> Set Text
deletesWithinDistance _ "" = Set.empty
deletesWithinDistance 0 word = Set.singleton word
deletesWithinDistance edits word =
  let
    ds = deletes word
  in
    Set.unions $ ds : map (deletesWithinDistance (edits - 1)) (Set.toList ds)

-- | Generate deletion candidates with breadth-first descent.
candidates :: Text -> [(Text, Int)]
candidates = candidatesBFS 0 . pure
 where
  candidatesBFS _ [] = []
  candidatesBFS depth words =
    map (,depth) words ++ candidatesBFS (depth + 1) (Set.toList $ concatMap deletes words)

-- | Restricted candidate generation with distance cap.
candidatesWithinDistance :: Int -> Text -> [Text]
candidatesWithinDistance maxDistance =
  map fst . takeWhile ((<= maxDistance) . snd) . candidates
