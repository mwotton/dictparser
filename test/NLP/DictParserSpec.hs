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

spec :: Spec
spec = describe "parsers" $ do
  it "should parse parts of speech" $ do
    parse pos "test" "verb and some rubbish\n"
      `shouldBe` (Right Verb)
