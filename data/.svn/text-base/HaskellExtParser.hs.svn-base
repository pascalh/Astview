module HaskellExtParser where

-- container
import Data.Tree (Tree(Node,rootLabel))

-- syb
import Data.Generics (Data)

-- base
import Unsafe.Coerce (unsafeCoerce)

-- local imports
import Language.Astview.Parser as Astview
import Language.Astview.DataTree

import Language.Haskell.Exts (parseFileContents)
import Language.Haskell.Exts.Parser (ParseResult(ParseOk))
import Language.Haskell.Exts.Syntax (Module)

haskellexts = Parser "Haskell" [".hs"] buildTreeHaskellExt

buildTreeHaskellExt :: String -> Tree String
buildTreeHaskellExt s = case parseHaskellExt s of
     Right ast -> flat $ data2tree (ast::Module)
     Left ParseError -> Node "ParseError" []

parseHaskellExt :: (Data a) => String -> Either ParseError a
parseHaskellExt s = case parseFileContents s of
  ParseOk p -> unsafeCoerce $ Right p
  _         -> Left ParseError
