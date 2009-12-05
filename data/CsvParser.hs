module CsvParser where

-- container
import Data.Tree (Tree(Node,rootLabel))

-- syb
import Data.Generics (Data)

-- base
import Unsafe.Coerce (unsafeCoerce)

-- local imports
import Language.Astview.Parser as Astview
import Language.Astview.DataTree

-- Parsec (CSV Parser)
import Text.ParserCombinators.Parsec as Parsec
import Data.Generics hiding (Infix)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Text.ParserCombinators.Parsec.Expr

csv = Parser "CSV" [] [".csv"] buildTreeCSV

-- CSV
buildTreeCSV :: String -> Tree String
buildTreeCSV s = case parseCSV s of
     Right ast -> flat $ data2tree (ast::[[String]])
     Left ParseError -> Node "ParseError" []

parseCSV :: (Data a) => String -> Either Astview.ParseError a
parseCSV s = case parseCSV' s of
   Right p -> unsafeCoerce $ Right p
   _       -> Left ParseError

-- Parsec (Simple CSV)
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSV' :: String -> Either Parsec.ParseError [[String]]
parseCSV' input = parse csvFile "(unknown)" input

