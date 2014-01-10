module NLP.DictParser.Types where

type Headword = String
type Translation = String
data Example = Translated String String
             | Untranslated String
               deriving (Show,Eq)

data Def a = Def Headword [(a, [(Translation, [Example])])]
            deriving (Show,Eq)

data Dict a = Dict {
  headers     :: [(String, String)],
  definitions :: [Def a]
} deriving (Show,Eq)
