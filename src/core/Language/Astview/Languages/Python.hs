module Language.Astview.Languages.Python (python) where
import Prelude hiding (span)
import Language.Astview.Language
import Language.Astview.DataTree (dataToAstIgnoreByExample)

import Language.Python.Version3.Parser(parseModule)
import qualified Language.Python.Common.SrcLocation as Py

import Data.Generics (Data,extQ)
import Data.Generics.Zipper(toZipper,down,query)

python :: Language
python = Language "Python" "Python" [".py"] parsePy

parsePy :: String -> Either Error Ast
parsePy s = case parseModule s [] of
  Right (m,_) -> Right $ dataToAstIgnoreByExample getSrcLoc
                                                  (undefined::Py.SrcSpan)
                                                  m
  Left e      -> Left $ ErrMessage (show e)

getSrcLoc :: Data t => t -> Maybe SrcSpan
getSrcLoc t = down (toZipper t) >>= query (def `extQ` atSpan) where

  def :: a -> Maybe SrcSpan
  def _ = Nothing

  atSpan :: Py.SrcSpan -> Maybe SrcSpan
  atSpan (Py.SpanPoint _ r c)             = Just $ position r c
  atSpan (Py.SpanCoLinear _ r sc ec)      = Just $ linear r sc ec
  atSpan (Py.SpanMultiLine _ sr sc er ec) = Just $ span sr sc er ec
  atSpan (Py.SpanEmpty)                   = Nothing
