module Haskell where

-- local imports
import Language.Astview.Language 

import qualified Language.Haskell.Exts.Parser as HsParser 
import Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.SrcLoc as HsSrcLoc

haskellexts :: Language (Module HsSrcLoc.SrcSpan) HsSrcLoc.SrcSpan
haskellexts = buildLanguage 
  "Haskell" 
  "Haskell" 
  [".hs",".lhs"] 
  parsehs 
  (Just toSrcLocHs)

parsehs :: String -> Either Error (Module HsSrcLoc.SrcSpan)
parsehs s = case HsParser.parse s of
    HsParser.ParseOk t   -> Right t
    HsParser.ParseFailed (HsSrcLoc.SrcLoc _ l c) m -> 
      Left $ ErrLocation (SrcPosition l c) m

toSrcLocHs :: HsSrcLoc.SrcSpan -> SrcLocation
toSrcLocHs (HsSrcLoc.SrcSpan _ c1 c2 c3 c4) = SrcSpan c1 c2 c3 c4
    
