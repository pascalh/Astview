module Haskell where

-- container
import Data.Tree (Tree(Node,rootLabel))

-- local imports
import Language.Astview.Language hiding (parse)
import qualified Language.Astview.SourceLocation as SrcLoc

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc

import Language.Astview.DataTree (data2tree)

haskellexts :: Language
haskellexts = Language 
  "Haskell" 
  "Haskell" 
  [".hs",".lhs"] 
  parHaskell
  (data2tree::Module SrcSpan ->Tree String)
  (Just toSrcLoc)
    
parHaskell :: String -> Either Error (Module SrcSpan)
parHaskell s =
  case parse s of
    ParseOk t   -> Right t
    ParseFailed (SrcLoc _ l c) m -> 
      Left $ ErrLocation (SrcLoc.SrcPosition l c) m


toSrcLoc :: Tree String -> [SrcLoc.SrcLocation]
toSrcLoc (Node "SrcSpan" cs) = 
  [SrcLoc.SrcSpan (read (to 1):: Int) 
           (read (to 2):: Int)
           (read (to 3):: Int)
           (read (to 4):: Int)
  ] 
  where to = rootLabel . (cs !!)
toSrcLoc _        = [] 
