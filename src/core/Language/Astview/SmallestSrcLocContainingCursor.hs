{-|
this module contains a brute force algorithm to compute the path to the smallest
source location in an abstract syntax tree which is surrounded by a given
cursor selection.
If multiple subtrees represent the smallest source location
 we select the biggest subtree. This is done by 'selectShortestPath'.
-}
module Language.Astview.SmallestSrcLocContainingCursor
  (smallestSrcLocContainingCursorPos)
where
import Data.Maybe(catMaybes)
import Data.Tree(flatten)
import Data.Function(on)
import Data.List (minimumBy)

import Language.Astview.Language

-- |selects the (shortest) path to the smallest source location containing
-- given cursor selection.
smallestSrcLocContainingCursorPos
  :: SrcLocation  -- ^ the cursor selection
  -> Ast  -- ^ the abstract syntax tree
  -> Maybe Path
smallestSrcLocContainingCursorPos sele =
 selectShortestPath . locsContainingSelection sele . findAllSrcLocations

-- |computes the shortest path in given association list. In case of an empty
-- list  'Nothing' is being returned.
selectShortestPath :: [(SrcLocation,Path)] -> Maybe Path
selectShortestPath []       = Nothing
selectShortestPath ps@(_:_) =
  Just $ minimumBy (compare `on` length) $ pathsToSmallestSrcLoc ps

-- |returns all paths with are associated with the smallest source location.
-- Precondition: input list is nonempty.
pathsToSmallestSrcLoc :: [(SrcLocation,Path)] -> [Path]
pathsToSmallestSrcLoc ps =
  let smallestSrcLoc = minimum $ map fst ps
  in map snd $ filter (\(s,_) -> s==smallestSrcLoc) ps

-- |extracts all source locations from abstract syntax tree
findAllSrcLocations :: Ast -> [(SrcLocation,Path)]
findAllSrcLocations (Ast ast) = (catMaybes . flatten . fmap getSrcLocPathPairs) ast

-- |returns the source locations associated to given node if existing
getSrcLocPathPairs :: AstNode -> Maybe (SrcLocation,Path)
getSrcLocPathPairs (AstNode _ Nothing  _ _) = Nothing
getSrcLocPathPairs (AstNode _ (Just s) p _) = Just (s,p)

-- |removes all source locations from list,
-- which are not surrounded by given cursor selection
locsContainingSelection :: SrcLocation -> [(SrcLocation,Path)] -> [(SrcLocation,Path)]
locsContainingSelection sele = filter (\(s,_) -> s >= sele)


