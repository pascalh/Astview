module Language.Astview.Languages.Haskell (haskell) where

-- local imports
import Language.Astview.Language hiding (parse)
import Language.Astview.DataTree (data2AstHoIg)

import Data.Generics (Data,extQ)
import Data.Generics.Zipper(toZipper,down',query)

import Parser
import Lexer hiding (getDynFlags, getSrcLoc, buffer)
import qualified DynFlags as GHC
import Outputable
import SrcLoc
import FastString
import StringBuffer
import qualified GHC

import GHC.Paths (libdir)

import System.IO.Unsafe

haskell :: Language
haskell = Language "Haskell" "Haskell" [".hs"] parsehs

parsehs :: String -> Either Error Ast
parsehs s =
  case runParser s parseModule of
    POk _ parsed -> Right (data2AstHoIg getSrcLoc (undefined :: SrcSpan) parsed)
    PFailed ss msg -> Left $ makeError ss (showSDocUnsafe msg)

makeError :: SrcSpan -> String -> Error
makeError ss s =
  case ss of
    RealSrcSpan real -> ErrLocation (ghcss2ss real) s
    _ -> ErrMessage s


ghcss2ss :: RealSrcSpan -> SrcLocation
ghcss2ss real
         = let start = realSrcSpanStart real
               end   = realSrcSpanEnd real
           in SrcSpan (srcLocLine start)
                      (srcLocCol start)
                      (srcLocLine end)
                      (srcLocCol end)

runParser :: String -> P a -> ParseResult a
runParser str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState (unsafePerformIO getDynFlags) buffer location


getDynFlags :: IO GHC.DynFlags
getDynFlags =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) GHC.getSessionDynFlags

getSrcLoc :: Data t => t -> Maybe SrcLocation
getSrcLoc t = down' (toZipper t) >>= query (def `extQ` atSpan) where

  def :: a -> Maybe SrcLocation
  def _ = Nothing

  atSpan :: GHC.SrcSpan -> Maybe SrcLocation
  atSpan (GHC.RealSrcSpan ss) = Just $ ghcss2ss ss
  atSpan _ = Nothing
