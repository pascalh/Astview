module Haskell where

-- container
import Data.Tree (Tree(Node,rootLabel))

-- local imports
import Language.Astview.Language hiding (parse)
import qualified Language.Astview.SourceLocation as SrcLoc

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc

haskellexts :: Language
haskellexts = Language 
  "Haskell" 
  "Haskell" 
  [".hs",".lhs"] 
  parHaskell
  toSrcLoc
    
parHaskell :: String -> Either Error (Module SrcSpan)
parHaskell s =
  case parse s of
    ParseOk t   -> Right t
    ParseFailed (SrcLoc _ l c) m -> 
      Left $ ErrLocation (SrcLoc.SrcPosition l c) m


toSrcLoc :: Tree String -> Maybe SrcLoc.SrcLocation
toSrcLoc (Node "SrcSpan" [_,c1,c2,c3,c4]) = 
  Just $ SrcLoc.SrcSpan (to c1) (to c2) (to c3) (to c4)
   where 
     to :: Tree String -> Int
     to = read . rootLabel 
toSrcLoc _        = Nothing 
