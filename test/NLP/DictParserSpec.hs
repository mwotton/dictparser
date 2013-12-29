{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.DictParserSpec where
-- import           Data.ByteString.Char8         (ByteString, lines, readFile,
--                                                unpack)
import           NLP.DictParser.Internal
import           Prelude                       hiding (lines, readFile)
import           Test.Hspec
import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec (parse)

dirname :: String
dirname = reverse $ dropWhile (/= '/') $ reverse __FILE__

instance Eq Text.Parsec.Error.ParseError where
  a == b = (show a) == (show b)

testP p = parse p "test"

spec :: Spec
spec = describe "parsers" $ do
  describe "primitives" $ do
    it "standard usage of line" $ do
      testP line "foo\n" `shouldBe` (Right "foo")
    it "can cope with empty lines" $ do
      testP line "\n" `shouldBe` (Right "")
  describe "should parse parts of speech" $ do
    it "should handle verbs" $ do
      testP pos  "* verb\n" `shouldBe` (Right Verb)
    it "should handle nouns" $ do
      testP pos "* noun\n" `shouldBe` (Right Noun)
    it "should handle extensions" $ do
      testP pos "* noun and some crap\n" `shouldBe` (Right Noun)
