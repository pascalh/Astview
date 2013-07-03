{-# LANGUAGE Rank2Types #-}
{-

This File exports the list of known parsers for astview.
You can extend the list with your own parsers 

-}

module Language.Astview.Languages where

-- -- local imports
import Language.Astview.Language (Language)
import Haskell  

-- |  astview
knownLanguages :: [Language]
knownLanguages = [haskellexts]
{-
langHaskell :: Lang
langHaskell = HaskellLang haskellexts

-- |a simple wrapper type to avoid heterogenous lists
data Lang = HaskellLang (Language (Hs.Module HsSrcLoc.SrcSpan) HsSrcLoc.SrcSpan)

data Ast = HsAst (Hs.Module HsSrcLoc.SrcSpan) deriving (Data,Typeable)

data SrcLoc = HsSrcLoc HsSrcLoc.SrcSpan deriving (Typeable)

langName :: Lang -> String
langName (HaskellLang l) = name l

langExts  :: Lang -> [String]
langExts (HaskellLang l) = exts l

langParse  :: Lang -> String  -> Either Error Ast
langParse (HaskellLang l) str = either Left (Right. HsAst) $ parse l str

langSyntax :: Lang -> String
langSyntax (HaskellLang l) = syntax l

langToSrcLoc :: Lang -> Maybe (SrcLoc-> SrcLocation)
langToSrcLoc (HaskellLang l) = do
  f <- toSrcLoc l  
  return $ \(HsSrcLoc x) -> f x

sybFoo :: (SrcLoc -> SrcLocation) -> Lang -> GenericQ ( Maybe SrcLocation)
sybFoo f (HaskellLang _) = fmap (f . HsSrcLoc) . cast

-}

