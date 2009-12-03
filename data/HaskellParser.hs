{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellExtParser where

-- container
import Data.Tree (Tree(Node,rootLabel))

-- syb
import Data.Generics (Data)

-- local imports
import Language.Astview.Parser as Astview
import Language.Astview.DataTree

import Language.Haskell.Exts 
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

import HigherOrderParse

haskellexts = Parser "Haskell" [".hs"] buildTreeH

-- higher-order build-tree 
-- (for syb we need to help the compiler about the type of data2tree)
buildTreeH = buildTreeGen (parseFileContents) (data2tree::Module->Tree String)

-- mapping the Errortype to out result type (with better error-msg)
instance Result (ParseResult a) a where
  from (ParseOk p) = Right p
  from (ParseFailed sloc s) = Left ("\""++s++"\""++" at <"++ (show $ srcLine sloc) ++","++(show $ srcColumn sloc)++">")
