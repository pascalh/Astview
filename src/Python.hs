module Python (python) where

import Language.Astview.Language 
import Language.Astview.DataTree (data2AstHoIg)

import Language.Python.Version3.Parser(parseModule)
import qualified Language.Python.Common.SrcLocation as Py

import Data.Generics (Data,extQ)
import Data.Generics.Zipper(toZipper,down,query)

python :: Language 
python = Language "Python" "Python" [".py"] parsePy 

parsePy :: String -> Either Error Ast 
parsePy s = case parseModule s [] of
  Right (m,_) -> Right $ data2AstHoIg getSrcLoc (undefined::Py.SrcSpan) m 
  Left e      -> Left $ ErrMessage (show e)

getSrcLoc :: Data t => t -> Maybe SrcLocation
getSrcLoc t = down (toZipper t) >>= query (def `extQ` atSpan) where

  def :: a -> Maybe SrcLocation
  def _ = Nothing

  atSpan :: Py.SrcSpan -> Maybe SrcLocation
  atSpan (Py.SpanPoint _ r c)             = Just $ SrcSpan r c r c
  atSpan (Py.SpanCoLinear _ r sc ec)      = Just $ SrcSpan r sc r ec
  atSpan (Py.SpanMultiLine _ sr sc er ec) = Just $ SrcSpan sr sc er ec
  atSpan (Py.SpanEmpty)                   = Nothing
