module Language.Astview.Languages.HaskellCore (haskellCore) where
import Prelude hiding (span)
import Data.Generics (Data,extQ)
import Data.Generics.Zipper(toZipper,down',query)

import Language.Astview.Language hiding (parse)
import Language.Astview.DataTree (dataToAstIgnoreByExample)

import Parser
import Lexer hiding (getDynFlags, getSrcLoc, buffer)
import qualified DynFlags as GHC
import Outputable
import qualified SrcLoc
import FastString
import StringBuffer
import qualified GHC

import GHC.Paths (libdir)

import System.IO.Unsafe

haskellCore :: Language
haskellCore = Language "HaskellCore" "Haskell" [".hs"] parsehs

parsehs :: String -> Either Error Ast
parsehs s =
  case runParser s parseModule of
    POk _ parsed -> Right (dataToAstIgnoreByExample getSrcLoc (undefined :: SrcSpan) parsed)
    PFailed ss msg -> Left $ makeError ss (showSDocUnsafe msg)

makeError :: SrcLoc.SrcSpan -> String -> Error
makeError ss s =
  case ss of
    SrcLoc.RealSrcSpan real -> ErrLocation (ghcss2ss real) s
    _ -> ErrMessage s


ghcss2ss :: SrcLoc.RealSrcSpan -> SrcSpan
ghcss2ss real
         = let start = SrcLoc.realSrcSpanStart real
               end   = SrcLoc.realSrcSpanEnd real
           in span (SrcLoc.srcLocLine start)
                   (SrcLoc.srcLocCol start)
                   (SrcLoc.srcLocLine end)
                   (SrcLoc.srcLocCol end)

runParser :: String -> P a -> ParseResult a
runParser str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = SrcLoc.mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState (unsafePerformIO getDynFlags) buffer location


getDynFlags :: IO GHC.DynFlags
getDynFlags =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) GHC.getSessionDynFlags

getSrcLoc :: Data t => t -> Maybe SrcSpan
getSrcLoc t = down' (toZipper t) >>= query (def `extQ` atSpan) where

  def :: a -> Maybe SrcSpan
  def _ = Nothing

  atSpan :: GHC.SrcSpan -> Maybe SrcSpan
  atSpan (GHC.RealSrcSpan ss) = Just $ ghcss2ss ss
  atSpan _ = Nothing
