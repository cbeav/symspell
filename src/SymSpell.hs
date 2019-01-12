module SymSpell
  ( fromList
  , suggest
  , SymSpell(..)
  , SymSpellConfig(..)
  , Suggestion(..)
  , Verbosity(..)
  ) where

import ClassyPrelude hiding (fromList)
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

data SymSpellConfig
  = SymSpellConfig
  { symSpellConfigMaxEditDistance :: !Int
  , symSpellConfigPrefixLength    :: !Int
  , symSpellConfigCompactLevel    :: !Int
  } deriving (Eq, Show)

instance Default SymSpellConfig where
  def = SymSpellConfig 2 7 5

data SymSpell
  = SymSpell
  { symSpellConfig      :: !SymSpellConfig
  , symSpellDeletes     :: !(HashMap Word32 (Set Text))
  , symSpellFrequencies :: !(HashMap Text Int)
  } deriving (Eq, Show)

data Suggestion
  = Suggestion
  { suggestionWord     :: !Text
  , suggestionDistance :: !Int
  } deriving (Eq, Show)

fromList :: SymSpellConfig -> [(Text, Int)] -> SymSpell
fromList symSpellConfig@SymSpellConfig{..} items =
  let
    hash = symSpellHash symSpellConfig
    words = map fst items
    deleteHashes = toList . Set.map hash . deletesPrefix symSpellConfig
    deleteMap w = HashMap.fromList . map (,Set.singleton w) $ deleteHashes w
    symSpellFrequencies = HashMap.fromList items
    symSpellDeletes = foldr (HashMap.unionWith Set.union . deleteMap) HashMap.empty words
  in
    SymSpell{..}

suggest :: SymSpell -> Int -> Text -> Verbosity -> [Suggestion]
suggest SymSpell{..} maxDistance input verbosity =
  let
    hash = symSpellHash symSpellConfig
    prefixLength = symSpellConfigPrefixLength symSpellConfig
    searchSpace = candidatesWithinDistance maxDistance $ take prefixLength input
    matchesFor candidate = HashMap.lookupDefault Set.empty (hash candidate) symSpellDeletes
    matches = toList . Set.unions $ map matchesFor searchSpace
    distances = map (id &&& damerauLevenshtein input) matches
    results = map (uncurry Suggestion) $ sortOn (Down . snd) $ filter ((<= maxDistance) . snd) distances
  in
    case (results, verbosity) of
      ([], _) -> []
      (top:_, Closest) -> takeWhile ((== suggestionDistance top) . suggestionDistance) results
      (top:_, Top) -> [top]
      _ -> results

symSpellHash :: SymSpellConfig -> Text -> Word32
symSpellHash SymSpellConfig{..} = maskedHash (compactMask symSpellConfigCompactLevel)

clip :: Ord a => a -> a -> a -> a
clip a b = (a `max`) . (`min` b)

compactMask :: Int -> Word32
compactMask level =
  shiftL (shiftR (maxBound :: Word32) (3 + clip 0 16 level)) 2

maskedHash :: Word32 -> Text -> Word32
maskedHash mask str =
  let
    mixChar c = (*16777619) . (`xor` unsafeCoerce c)
    lenMask = min 3 . unsafeCoerce $ length str
  in
    (.|. lenMask) . (.&. mask) $ foldr mixChar 2166136261 str

deletesPrefix :: SymSpellConfig -> Text -> Set Text
deletesPrefix SymSpellConfig{..} =
  deletesWithinDistance symSpellConfigMaxEditDistance . take symSpellConfigPrefixLength

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
candidates = candidatesRec 0 . pure

candidatesWithinDistance :: Int -> Text -> [Text]
candidatesWithinDistance maxDistance =
  map fst . takeWhile ((<= maxDistance) . snd) . candidates

candidatesRec :: Int -> [Text] -> [(Text, Int)]
candidatesRec _ [] = []
candidatesRec depth words =
  map (,depth) words ++ candidatesRec (depth + 1) (Set.toList $ concatMap deletes words)
