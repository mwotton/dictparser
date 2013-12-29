module NLP.DictParser.Internal where

import           Control.Applicative           hiding (many, (<|>))
import           Data.Either
import           Data.Maybe
import           Debug.Trace
import           Text.ParserCombinators.Parsec

data POS = Verb
         | Adjective
         | Noun
         | Exclamation
         | Informal
         | Conjugation
         | Number
         | Adverb
         | Pronoun
         | Broken String
  deriving (Show,Eq)

type Headword = String
type Translation = String
data Example = Translated String String
             | Untranslated String
               deriving (Show,Eq)

data Def = Def Headword [(POS, [(Translation, [Example])])]
           deriving (Show,Eq)

data Dict = Dict {
  headers     :: [(String, String)],
  definitions :: [Def]
} deriving (Show,Eq)


separator = do
  string "_____"
  newline
  newline

manyTill1 p end = (:) <$> p <*> p `manyTill` end

line1 = (:) <$> noneOf "\n" <*> line
line =  manyTill anyChar (try newline)

acceptedHeaders :: [String]
acceptedHeaders = ["short",
                   "info",
                   "url"]

header = do
  trace ("trying headers") (return ())
  string "00-database-"
  header <- choice $ map string acceptedHeaders
  trace (show ("headers",header)) (return ())
  newline
  descriptions <- many dashLine
  newline
  return (header, unlines descriptions)

dashLine = string "-" *> spaces *> line
-- starLine = string "* " *> line
equalLine = string "=" *> spaces *> line

resync = anyChar `manyTill` (try (eof <|> (separator *> return ())))

dictFile :: GenParser Char st Dict
dictFile = do
  separator
  headers <- many (header <* separator)
  defs <- many ((Right <$> try defP) <|> (Left <$> resync))
  trace (show $ lefts defs) return ()
  return $ Dict headers (rights defs)

defP :: GenParser Char st Def
defP = do
  d <- Def  <$> line1 <*> (many withPOS) <?> "Def"
  newline
  Text.ParserCombinators.Parsec.optional separator
  return d
-- defP = Def  <$> line <*> (many withPOS) <?> "Def"

withPOS :: GenParser Char st (POS, [(Translation, [Example])])
withPOS = (,) <$> pos <*> many translation

textline = (:) <$> noneOf "=*-\n_" <*> line

pos = do
  string "*"
  spaces
  choice (goodParts ++ [Broken <$> line])

  where goodParts = map (\(x,res) -> try (string x *> line *> return res) ) parts

parts = [("verb", Verb)
        ,("noun", Noun)
        ,("number", Number)
        ,("khẩu ngữ", Informal)
        ,("adj", Adjective)
        ,("adv", Adverb)
        ,("excl", Exclamation)
        ,("pronoun", Pronoun)
        ,("conj", Conjugation)
        ]


translation :: GenParser Char st (Translation, [Example])
translation = (,) <$> dashLine <*> many example <?> "example"

example :: GenParser Char st Example
example = do
  string "="
  spaces
  ex <- line
  return $ case span (/='+') ex of
    (a,[]) -> Untranslated a
    (a,(_:b))  -> Translated a b

--  (,) <$> manyTill1 (noneOf "\n+") (try $ string "+") <*> line
