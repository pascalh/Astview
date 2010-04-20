{-

This File exports the list of known parsers for astview.
You can extend the list with your own parsers as proposed with the 
CustomParsers.hs module and the concatenation of the list.

Beware, this file will be overwritten when updating the package.

-}

module Parsers where

-- container
import Data.Tree (Tree(Node,rootLabel),drawTree)

-- -- local imports
import Language.Astview.Parser (Parser (..))
import Language.Astview.DataTree (flat,data2tree)
import HaskellExtParser  -- requires haskell-src-exts
import CsvParser      -- requires parsec
import ExprParser     -- requires parsec

import HigherOrderParse

-- | Main export for dynamic interpretation by astview
parsers :: [Parser]
parsers = [linesAndWords 
          ,csv
          ,expr
          ,haskellexts
          ]

-- --------------------------------------------------------

-- | Define a custom parser
linesAndWords :: Parser
linesAndWords = Parser "Lines and Words" [] [".law"] buildTreeLaw

buildTreeLaw = buildTreeGen (Just . map words . lines) data2tree
