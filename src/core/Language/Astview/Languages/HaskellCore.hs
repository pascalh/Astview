module Language.Astview.Languages.HaskellCore (haskellCore) where
import           Prelude                   hiding (span)

import           Data.Generics             (Data, Typeable, ext1Q, extQ, gmapQ,
                                            showConstr, toConstr, typeOf)
import           Data.Generics.Zipper      (down', query, toZipper)
import           Data.Tree                 (Tree (Node))

import           Language.Astview.DataTree (manual)
import           Language.Astview.Language hiding (parse)

import qualified DynFlags                  as GHC
import           FastString
import qualified GHC
import           Lexer                     hiding (buffer, getDynFlags,
                                            getSrcLoc)
import qualified OccName                   as GHC
import           Outputable
import qualified Outputable                as GHC
import           Parser
import qualified RdrName                   as GHC
import qualified SrcLoc                    as GHC
import           StringBuffer

import           GHC.Paths                 (libdir)

import           System.IO.Unsafe

haskellCore :: Language
haskellCore = Language "HaskellCore" "Haskell" [".hs"] parsehs

parsehs :: String -> Either Error Ast
parsehs s =
  case runParser s parseModule of
    POk _ parsed -> Right (coreToAst parsed)
    PFailed ss msg -> Left $ makeError ss (showSDoc (unsafePerformIO getDynFlags) msg)

makeError :: GHC.SrcSpan -> String -> Error
makeError ss s =
  case ss of
    GHC.RealSrcSpan real -> ErrLocation (ghcss2ss real) s
    _ -> ErrMessage s


ghcss2ss :: GHC.RealSrcSpan -> SrcSpan
ghcss2ss real
         = let start = GHC.realSrcSpanStart real
               end   = GHC.realSrcSpanEnd real
           in span (GHC.srcLocLine start)
                   (GHC.srcLocCol start)
                   (GHC.srcLocLine end)
                   (GHC.srcLocCol end)

runParser :: String -> P a -> ParseResult a
runParser str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = GHC.mkRealSrcLoc (mkFastString filename) 1 1
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

-- * typed ast to untyped ast

coreToAst :: (Data t,Typeable t) => t -> Ast
coreToAst = manual worker where
  worker :: (Data t,Typeable t) => t -> Tree (Maybe AstNode)
  worker term
   | term `equalTypes` (undefined :: GHC.SrcSpan) = Node Nothing []
   | otherwise   = (gdefault `ext1Q` atL `extQ` atRdrName `extQ` atOccName `extQ` atString) term where

      atString :: String -> Tree (Maybe AstNode)
      atString s = Node (Just $ AstNode s Nothing [] Identificator) []

      atRdrName :: GHC.RdrName -> Tree (Maybe AstNode)
      atRdrName rdr = Node (Just $ AstNode (rdrName2String rdr) (getSrcLoc rdr) [] Identificator) []

      atOccName :: GHC.OccName -> Tree (Maybe AstNode)
      atOccName o = Node (Just $ AstNode (GHC.occNameString o) (getSrcLoc o) [] Identificator) []

      atL :: Data t => GHC.GenLocated GHC.SrcSpan t -> Tree (Maybe AstNode)
      atL (GHC.L _ a) = worker a

      gdefault :: Data t => t -> Tree (Maybe AstNode)
      gdefault x = Node (Just n) cs where

        n :: AstNode
        n = AstNode (showConstr $ toConstr x) (getSrcLoc x) [] Operation

        cs = gmapQ worker x

rdrName2String :: GHC.RdrName -> String
rdrName2String r =
  case GHC.isExact_maybe r of
    Just n  -> showGhc n
    Nothing ->
      case r of
        GHC.Unqual _occ       -> GHC.occNameString $ GHC.rdrNameOcc r
        GHC.Qual modname _occ -> GHC.moduleNameString modname ++ "."
                            ++ GHC.occNameString (GHC.rdrNameOcc r)
        GHC.Orig _ _          -> error "GHC.Orig introduced after renaming"
        GHC.Exact _           -> error "GHC.Exact introduced after renaming"

showGhc :: (GHC.Outputable a) => a -> String
showGhc = GHC.showPpr GHC.unsafeGlobalDynFlags

-- |returns whether both values are of the same type
equalTypes :: (Typeable b1,Typeable b2)  => b1 -> b2  -> Bool
equalTypes t1 t2 = typeOf t1 == typeOf t2
