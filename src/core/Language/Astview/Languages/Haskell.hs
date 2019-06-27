module Language.Astview.Languages.Haskell (haskellExts) where
import           Prelude                                hiding (span)

import           Language.Astview.DataTree              (dataToAstIgnoreByExample)
import           Language.Astview.Language

import           Data.Generics                          (Data, extQ)
import           Data.Generics.Zipper                   (down', query, toZipper)

import           Language.Haskell.Exts.Parser           (ParseResult (..),parseModule)
import qualified Language.Haskell.Exts.SrcLoc           as HsSrcLoc

haskellExts :: Language
haskellExts = Language "Haskell" "Haskell" [".hs"] (PureParser parsehs)

parsehs :: String -> Either Error Ast
parsehs s = case parseModule s of
  ParseOk t  -> Right $ dataToAstIgnoreByExample getSrcLoc
                                                 (undefined::HsSrcLoc.SrcSpanInfo)
                                                 t
  ParseFailed (HsSrcLoc.SrcLoc _ l c) m -> Left $ ErrLocation (position l c) m

getSrcLoc :: Data t => t -> Maybe SrcSpan
getSrcLoc t = down' (toZipper t) >>= query (def `extQ` atSpan) where

  def :: a -> Maybe SrcSpan
  def _ = Nothing

  atSpan :: HsSrcLoc.SrcSpanInfo -> Maybe SrcSpan
  atSpan (HsSrcLoc.SrcSpanInfo (HsSrcLoc.SrcSpan _ c1 c2 c3 c4) _) =
    Just $ span c1 c2 c3 c4
