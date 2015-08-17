module Language.Astview.Languages.Haskell (haskellexts) where

-- local imports
import Language.Astview.Language hiding (parse)
import Language.Astview.DataTree (dataToAstIgnoreByExample)

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.SrcLoc as HsSrcLoc

import Data.Generics (Data,extQ)
import Data.Generics.Zipper(toZipper,down',query)

haskellexts :: Language
haskellexts = Language "Haskell" "Haskell" [".hs",".lhs"] parsehs

parsehs :: String -> Either Error Ast
parsehs s = case parse s :: ParseResult (Module HsSrcLoc.SrcSpan) of
  ParseOk t  -> Right $ dataToAstIgnoreByExample getSrcLoc 
                                                 (undefined::HsSrcLoc.SrcSpan) 
                                                 t
  ParseFailed (HsSrcLoc.SrcLoc _ l c) m -> Left $ ErrLocation (position l c) m

getSrcLoc :: Data t => t -> Maybe SrcLocation
getSrcLoc t = down' (toZipper t) >>= query (def `extQ` atSpan) where

  def :: a -> Maybe SrcLocation
  def _ = Nothing

  atSpan :: HsSrcLoc.SrcSpan -> Maybe SrcLocation
  atSpan (HsSrcLoc.SrcSpan _ c1 c2 c3 c4) = Just $ SrcSpan c1 c2 c3 c4
