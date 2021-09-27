import System.Environment
import Text.ParserCombinators.ReadP

main = do
  args <- getArgs


trimComments :: ReadP Char
trimComments = manyTill get satisfy (== "#")


tokenize :: ReadP String
tokenize = do
