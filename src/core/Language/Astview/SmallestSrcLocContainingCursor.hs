{-|
this module contains a brute force algorithm to compute the smallest 
source location in a tree which is surrounded by a given cursor selection.
The size of a source location is the size of the represented interval.
-}
module Language.Astview.SmallestSrcLocContainingCursor 
  (smallestSrcLocContainingCursorPos) 
where
import Data.Maybe(catMaybes)
import Data.Tree(flatten)

import Language.Astview.Pathlist(PathList,fromList)
import Language.Astview.Language

-- |selects the smallest source location containing given cursor selection
smallestSrcLocContainingCursorPos :: CursorSelection -> Ast -> PathList
smallestSrcLocContainingCursorPos sele = 
  fromList . locsContainingSelection sele . findAllSrcLocations

-- |extracts all source locations from abstract syntax tree
findAllSrcLocations :: Ast -> [(SrcLocation,[Int])]
findAllSrcLocations (Ast ast) = (catMaybes . flatten . fmap getSrcLocPathPairs) ast

-- |returns the source locations associated to given node if existing  
getSrcLocPathPairs :: AstNode -> Maybe (SrcLocation,[Int])
getSrcLocPathPairs (AstNode _ Nothing  _ _) = Nothing
getSrcLocPathPairs (AstNode _ (Just s) p _) = Just (s,p)

-- |removes all source locations from list, 
-- which are not surrounded by given cursor selection
locsContainingSelection :: CursorSelection -> [(SrcLocation,[Int])] -> [(SrcLocation,[Int])] 
locsContainingSelection sele = filter (\(s,_) -> s >= selectionToSpan sele)
