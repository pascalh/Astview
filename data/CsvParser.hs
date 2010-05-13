
module CsvParser where
{-
-- container
import Data.Tree (Tree(Node,rootLabel))

-- local imports
import Language.Astview.Parser as Astview

-- Parsec (CSV Parser)
import Data.Generics hiding (Infix)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import HigherOrderParse

csv = Parser "CSV" [] [".csv"] buildTreeCSV

buildTreeCSV = 
  buildTreeGen 
    (parse csvFile "(unknown)")
    (data2tree::[[String]] -> Tree String)

-- Parsec (Simple CSV)
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'
-}
