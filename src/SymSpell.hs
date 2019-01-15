{-# LANGUAGE DataKinds #-}
module SymSpell
  ( fromList
  , suggest
  , SymSpell(..)
  , SymSpellConfig(..)
  , Suggestion(..)
  , Verbosity(..)
  ) where

import ClassyPrelude hiding (fromList)
import Closed
import Control.Arrow ((&&&))
import Data.Bits
import Data.Default
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Metrics (damerauLevenshtein)
import Unsafe.Coerce (unsafeCoerce)

data Verbosity
  = Top
  | Closest
  | All
  deriving (Eq, Show)

type CompactionLevel = Closed 0 16

data SymSpellConfig
  = SymSpellConfig
  { symSpellConfigMaxEditDistance :: !Int
  , symSpellConfigPrefixLength    :: !Int
  , symSpellConfigCompactionLevel :: !CompactionLevel
  } deriving (Eq, Show)

instance Default SymSpellConfig where
  def = SymSpellConfig 2 7 5

type PrefixFunction = Text -> Text

type HashFunction = Text -> Word32

data SymSpell
  = SymSpell
  { symSpellPrefix      :: !PrefixFunction
  , symSpellHash        :: !HashFunction
  , symSpellDeletes     :: !(HashMap Word32 (Set Text))
  , symSpellFrequencies :: !(HashMap Text Int)
  }

data Suggestion
  = Suggestion
  { suggestionWord     :: !Text
  , suggestionDistance :: !Int
  } deriving (Eq, Show)

fromList :: SymSpellConfig -> [(Text, Int)] -> SymSpell
fromList SymSpellConfig{..} items =
  let
    symSpellPrefix = take symSpellConfigPrefixLength
    symSpellHash = compactHash symSpellConfigCompactionLevel
    words = map fst items
    deleteHashes = toList . Set.map symSpellHash . deletesPrefix symSpellPrefix symSpellConfigMaxEditDistance
    deleteMap w = HashMap.fromList . map (,Set.singleton w) $ deleteHashes w
    symSpellFrequencies = HashMap.fromList items
    symSpellDeletes = foldr (HashMap.unionWith Set.union . deleteMap) HashMap.empty words
  in
    SymSpell{..}

suggest :: SymSpell -> Int -> Text -> Verbosity -> [Suggestion]
suggest SymSpell{..} maxDistance input verbosity =
  let
    searchSpace = candidatesWithinDistance maxDistance $ symSpellPrefix input
    matchesFor candidate = HashMap.lookupDefault Set.empty (symSpellHash candidate) symSpellDeletes
    matches = toList . Set.unions $ map matchesFor searchSpace
    distances = map (id &&& damerauLevenshtein input) matches
    results = map (uncurry Suggestion) $ sortOn (Down . snd) $ filter ((<= maxDistance) . snd) distances
  in
    case (results, verbosity) of
      ([], _) -> []
      (top:_, Closest) -> takeWhile ((== suggestionDistance top) . suggestionDistance) results
      (top:_, Top) -> [top]
      _ -> results

compactHash :: CompactionLevel -> HashFunction
compactHash level str =
  let
    mask = shiftL (shiftR (maxBound :: Word32) (3 + fromInteger (getClosed level))) 2
    mixChar c = (*16777619) . (`xor` unsafeCoerce c)
    lenMask = min 3 . unsafeCoerce $ length str
  in
    (.|. lenMask) . (.&. mask) $ foldr mixChar 2166136261 str

deletesPrefix :: PrefixFunction -> Int -> Text -> Set Text
deletesPrefix prefix distance = deletesWithinDistance distance . prefix

deletes :: Text -> Set Text
deletes word =
  Set.fromList $ map (withoutN word) [0..(length word - 1)]
 where
  withoutN str n = take n str ++ drop (n + 1) str

deletesWithinDistance :: Int -> Text -> Set Text
deletesWithinDistance 0 _  = Set.empty
deletesWithinDistance _ "" = Set.empty
deletesWithinDistance edits word =
  let
    ds = deletes word
  in
    Set.unions $ ds : map (deletesWithinDistance (edits - 1)) (Set.toList ds)

candidates :: Text -> [(Text, Int)]
candidates = candidatesBFS 0 . pure
 where
  candidatesBFS _ [] = []
  candidatesBFS depth words =
    map (,depth) words ++ candidatesBFS (depth + 1) (Set.toList $ concatMap deletes words)

candidatesWithinDistance :: Int -> Text -> [Text]
candidatesWithinDistance maxDistance =
  map fst . takeWhile ((<= maxDistance) . snd) . candidates
