module SymSpellSpec
  ( spec
  ) where

import ClassyPrelude hiding (fromList)
import Test.Hspec

import Data.Default
import SymSpell

spec :: Spec
spec = do

  let
    dictionary = [("shop", 123), ("ship", 456), ("shit", 789), ("word", 314), ("hippopotamus", 1)]

  symSpell <- runIO $ fromList def dictionary

  describe "suggest word" $ do
    it "works for Top with edit distance 0" $
      suggest symSpell 0 Top "word" `shouldBe`
        [ SymSpellSuggestion "word" 0
        ]

    it "works for Closest with edit distance 0" $
      suggest symSpell 0 Closest "word" `shouldBe`
        [ SymSpellSuggestion "word" 0
        ]

    it "works for All with edit distance 0" $
      suggest symSpell 0 All "word" `shouldBe`
        [ SymSpellSuggestion "word" 0
        ]

    it "works for Top with edit distance 1" $
      suggest symSpell 1 Top "word" `shouldBe`
        [ SymSpellSuggestion "word" 0
        ]

    it "works for Closest with edit distance 1" $
      suggest symSpell 1 Closest "word" `shouldBe`
        [ SymSpellSuggestion "word" 0
        ]

    it "works for All with edit distance 1" $
      suggest symSpell 1 All "word" `shouldBe`
        [ SymSpellSuggestion "word" 0
        ]


  describe "suggest shup" $ do
    it "works for Top with edit distance 1" $
      suggest symSpell 1 Top "shup" `shouldBe`
        [ SymSpellSuggestion "ship" 1
        ]

    it "works for Closest with edit distance 1" $
      suggest symSpell 1 Closest "shup" `shouldBe`
        [ SymSpellSuggestion "ship" 1
        , SymSpellSuggestion "shop" 1
        ]

    it "works for All with edit distance 1" $
      suggest symSpell 1 All "shup" `shouldBe`
        [ SymSpellSuggestion "ship" 1
        , SymSpellSuggestion "shop" 1
        ]

    it "works for Top with edit distance 2" $
      suggest symSpell 2 Top "shup" `shouldBe`
        [ SymSpellSuggestion "ship" 1
        ]

    it "works for Closest with edit distance 2" $
      suggest symSpell 2 Closest "shup" `shouldBe`
        [ SymSpellSuggestion "ship" 1
        , SymSpellSuggestion "shop" 1
        ]

    it "works for All with edit distance 2" $
      suggest symSpell 2 All "shup" `shouldBe`
        [ SymSpellSuggestion "ship" 1
        , SymSpellSuggestion "shop" 1
        , SymSpellSuggestion "shit" 2
        ]

  describe "suggest hippopotomus (exceeding prefix length)" $ do
    it "works for Top with edit distance 1" $
      suggest symSpell 1 Top "hippopotomus" `shouldBe`
        [ SymSpellSuggestion "hippopotamus" 1
        ]

    it "works for Closest with edit distance 1" $
      suggest symSpell 1 Closest "hippopotomus" `shouldBe`
        [ SymSpellSuggestion "hippopotamus" 1
        ]

    it "works for All with edit distance 1" $
      suggest symSpell 1 All "hippopotomus" `shouldBe`
        [ SymSpellSuggestion "hippopotamus" 1
        ]

  describe "suggest blah" $ do
    it "matches nothing for Top with edit distance 1" $
      suggest symSpell 1 Top "blah" `shouldBe` []

    it "matches nothing for Closest with edit distance 1" $
      suggest symSpell 1 Closest "blah" `shouldBe` []

    it "matches nothing for All with edit distance 1" $
      suggest symSpell 1 All "blah" `shouldBe` []

  describe "suggest blahblahblah (exceeding prefix length)" $ do
    it "matches nothing for Top with edit distance 1" $
      suggest symSpell 1 Top "blahblahblah" `shouldBe` []

    it "matches nothing for Closest with edit distance 1" $
      suggest symSpell 1 Closest "blahblahblah" `shouldBe` []

    it "matches nothing for All with edit distance 1" $
      suggest symSpell 1 All "blahblahblah" `shouldBe` []
