
module Csv where

-- container
import Data.Tree (Tree(Node,rootLabel))

-- local imports
import qualified Language.Astview.Language as L
import DataTree (data2tree)

-- Parsec (CSV Parser)
import Data.Generics hiding (Infix)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

csv = 
  L.Language 
    "CSV" 
    [] 
    [".csv"] 
    (const $ Left L.Err)
    (data2tree::[[String]] -> Tree String)
    Nothing
    Nothing
   
-- (parse csvFile "(unknown)")
    
-- Parsec (Simple CSV)
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

