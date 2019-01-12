import ClassyPrelude
import Data.Default
import SymSpell
import SymSpell.IO (fromFile)

main :: IO ()
main = do
  (file:word:_) <- getArgs
  symspell <- fromFile def $ unpack file
  print $ suggest symspell 1 word Top
