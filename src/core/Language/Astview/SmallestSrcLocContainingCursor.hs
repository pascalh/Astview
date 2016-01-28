{-|
This module contains a brute force algorithm to compute the associated subtree
for a given source span.
We need this in the GUI to associate text selections with a subtree of the ast.
-}
module Language.Astview.SmallestSrcLocContainingCursor
  (smallestSrcLocContainingCursorPos)
where
import Data.Maybe(catMaybes)
import Data.Tree(flatten)
import Data.Function(on)
import Data.List (minimumBy)

import Language.Astview.Language

{- |selects the shortest path to the subtree of @t@
 which is annotated by the smallest source span containing
 @s@ (if existing).
If multiple subtrees represent such source span the greatest subtree
is chosen.
 -}
smallestSrcLocContainingCursorPos
  :: SrcSpan  -- ^ source span @s@
  -> Ast  -- ^ tree @t@
  -> Maybe Path
smallestSrcLocContainingCursorPos sele =
 selectShortestPath . locsContainingSelection sele . findAllSrcSpans

-- |computes the shortest path in given association list. In case of an empty
-- list  'Nothing' is being returned.
selectShortestPath :: [(SrcSpan,Path)] -> Maybe Path
selectShortestPath []       = Nothing
selectShortestPath ps@(_:_) =
  Just $ minimumBy (compare `on` length) $ pathsToSmallestSrcLoc ps

-- |returns all paths with are associated with the smallest source spans.
-- Precondition: input list is nonempty.
pathsToSmallestSrcLoc :: [(SrcSpan,Path)] -> [Path]
pathsToSmallestSrcLoc ps =
  let smallestSrcLoc = minimum $ map fst ps
  in map snd $ filter (\(s,_) -> s==smallestSrcLoc) ps

-- |extracts all source spans from abstract syntax tree
findAllSrcSpans :: Ast -> [(SrcSpan,Path)]
findAllSrcSpans (Ast ast) = (catMaybes . flatten . fmap getSrcLocPathPairs) ast

-- |returns the source spans associated to given node if existing
getSrcLocPathPairs :: AstNode -> Maybe (SrcSpan,Path)
getSrcLocPathPairs (AstNode _ Nothing  _ _) = Nothing
getSrcLocPathPairs (AstNode _ (Just s) p _) = Just (s,p)

-- |removes all source spans from list,
-- which are not surrounded by given cursor selection
locsContainingSelection :: SrcSpan -> [(SrcSpan,Path)] -> [(SrcSpan,Path)]
locsContainingSelection sele = filter (\(s,_) -> s >= sele)


