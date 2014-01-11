{-# LANGUAGE CPP #-}
module NLP.DictParser.Internal where

import           Control.Applicative           hiding (many, (<|>))
import           Data.Either
import           Data.List.Split               (splitOn)
import           NLP.DictParser.Types
import           Text.ParserCombinators.Parsec

separator = "_____\n\n"

manyTill1 p end = (:) <$> p <*> p `manyTill` end

line1 = (:) <$> noneOf "\n" <*> line
line =  manyTill anyChar (try newline)

acceptedHeaders :: [String]
acceptedHeaders = ["short",
                   "info",
                   "url"]

headerP = do
  string "00-database-"
  header <- choice $ map string acceptedHeaders
  newline
  descriptions <- many dashLine
  newline
  return (header, unlines descriptions)

dashLine = string "-" *> spaces *> line
-- starLine = string "* " *> line
equalLine = string "=" *> spaces *> line

-- resync = anyChar `manyTill` (try (eof <|> (separator *> return ())))


parseString :: String -> (Dict String, [String])
parseString input = (Dict (rights $ map (tryParse headerP) headersText) valid,
                   map show invalid)

  where (headersText,body) = span (isRight . tryParse headerP) . map strip $ splitOn separator input
        (invalid, valid) = partitionEithers $ map (tryParse defP) body
        isHeader x = True

-- this is awful
#if __GLASGOW_HASKELL__ < 772
isRight (Right _) = True
isRight _ = False
#endif

tryParse p body = case parse p "(none)" body of
  Left _ -> Left body
  Right x -> Right x

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

-- dictFile :: GenParser Char st (Dict String)
-- dictFile = do
--   separator
--   headers <- many (header <* separator)
--   defs <- many ((Right <$> try defP) <|> (Left <$> resync))
--   trace (show $ lefts defs) return ()
--   return $ Dict headers (rights defs)

defP :: GenParser Char st (Def String)
defP = Def  <$> line1 <*> (many withPOS) <?> "Def"

withPOS :: GenParser Char st (String, [(Translation, [Example])])
withPOS = (,) <$> pos <*> many translation

textline = (:) <$> noneOf "=*-\n_" <*> line

pos = do
  string "*"
  spaces
  line
  -- choice (goodParts ++ [Broken <$> line])

  -- where goodParts = map (\(x,res) -> try (string x *> line *> return res) ) parts



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
