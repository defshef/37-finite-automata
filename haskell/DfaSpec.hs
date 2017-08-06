import           Control.Exception (evaluate)
import           Dfa
import           Test.Hspec
import           Test.QuickCheck

sampleRules = [(1, 'a', 2), (1, 'b', 1),
              (2, 'a', 2), (2, 'b', 3),
              (3, 'a', 3), (3, 'b', 3)]

sampleDfa = Dfa {state = 1, acceptStates = [1, 3], rulebook = sampleRules}

modifiedDfa = sampleDfa {acceptStates = [3]}

main :: IO ()
main = hspec $ do
  describe "matchesRule" $ do
    it "matches when current,char match" $ matchesRule 1 (1, 'a', 2) 'a' `shouldBe` True
    it "does not match when current,char do not match" $ matchesRule 1 (1, 'a', 2) 'b' `shouldBe` False

  describe "maybeRule" $ do
    it "returns a rule when one matches" $ maybeRule 1 sampleRules 'a' `shouldBe` Just (1, 'a', 2)
    it "returns a rule when one matches" $ maybeRule 2 sampleRules 'b' `shouldBe` Just (2, 'b', 3)
    it "returns Nothing when none match" $ maybeRule 2 sampleRules 'c' `shouldBe` Nothing

  --describe "acceptChar" $ do
    --it "returns a new Dfa when new char matches" $ acceptChar sampleDfa 'a' `shouldBe` Just sampleDfa {state = 2}
    --it "returns Nothing when new char doesn't match" $ acceptChar sampleDfa 'c' `shouldBe` Nothing

  describe "acceptString with sampleDfa" $ do
    it "rejects 'aaa'" $ acceptString sampleDfa "aaa" `shouldBe` False
    it "accepts 'bbb'" $ acceptString sampleDfa "bbb" `shouldBe` True
    it "rejects 'ba'" $ acceptString sampleDfa "ba" `shouldBe` False
    it "rejects 'ba'" $ acceptString sampleDfa "ba" `shouldBe` False
    it "accepts 'ab'" $ acceptString sampleDfa "ab" `shouldBe` True
    it "accepts 'bab''" $ acceptString sampleDfa "bab" `shouldBe` True
    it "rejects 'baa''" $ acceptString sampleDfa "baa" `shouldBe` False
    it "accepts ''" $ acceptString sampleDfa "" `shouldBe` True

  describe "acceptString with modified Dfa" $ do
    it "rejects 'aaa'" $ acceptString modifiedDfa "aaa" `shouldBe` False
    it "rejects 'bbb'" $ acceptString modifiedDfa "bbb" `shouldBe` False
    it "rejects 'ba'" $ acceptString modifiedDfa "ba" `shouldBe` False
    it "rejects 'ba'" $ acceptString modifiedDfa "ba" `shouldBe` False
    it "accepts 'ab'" $ acceptString modifiedDfa "ab" `shouldBe` True
    it "accepts 'bab''" $ acceptString modifiedDfa "bab" `shouldBe` True
    it "rejects 'baa''" $ acceptString modifiedDfa "baa" `shouldBe` False
    it "rejects ''" $ acceptString modifiedDfa "" `shouldBe` False
