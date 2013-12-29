import           NLP.DictParser
import           Text.Parsec

main :: IO ()
main =  getContents >>= \c -> print (parse dictFile "text input" (c))
