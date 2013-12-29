module NLP.DictParser(dictFile) where

import           Control.Applicative           hiding (many)
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
  deriving Show

type Headword = String
type Translation = String
data Example = Translated String String
             | Untranslated String
               deriving Show

data Def = Def Headword [(POS, [(Translation, [Example])])]
           deriving Show

data Dict = Dict {
  headers     :: [(String, String)],
  definitions :: [Def]
} deriving Show


separator = do
  string "_____"
  newline
  newline

-- manyTill1 :: Stream s m Char => ParsecT s u m r -> ParsecT s u m e -> ParsecT s u m [r]
manyTill1 p end = (:) <$> p <*> p `manyTill` end

line =  (manyTill1 anyChar (try newline))

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

dictFile :: GenParser Char st Dict
dictFile = do
  separator
  headers <- many (header <* separator)
  defs <- many (defP <* Text.ParserCombinators.Parsec.optional separator)
  return $ Dict headers defs

defP :: GenParser Char st Def
defP = Def  <$> line <*> (many withPOS) <?> "Def"
-- defP = Def  <$> line <*> (many withPOS) <?> "Def"

withPOS :: GenParser Char st (POS, [(Translation, [Example])])
withPOS = (,) <$> pos <*> many translation <?> "POS"

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
    (a,b)  -> Translated a b

--  (,) <$> manyTill1 (noneOf "\n+") (try $ string "+") <*> line

main = do
  -- defs <- map parseChunk . splitOn "-----" <$> getContents
  -- print defs
  getContents >>= \c -> print (parse dictFile "text input" (c))
