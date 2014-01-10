import           NLP.DictParser

main :: IO ()
main =  getContents >>= \c -> print (parseString c)
