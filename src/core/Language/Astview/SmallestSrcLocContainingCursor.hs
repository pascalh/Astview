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
import qualified Data.Set as S
import Data.Function(on)  
import Data.List (minimumBy)

import Language.Astview.Language


type Path = [Int]

-- |a pathlist stores a source location and paths to all subtrees annotated
-- with the source location
data PathList 
  = Empty -- ^ the empty list
  | PathList  SrcLocation (S.Set Path) 
    -- ^ a source location and their associated tree positions
  deriving (Show,Eq)

singleton :: (SrcLocation,Path) -> PathList
singleton (x,p) = ins (x,p) Empty 

ins :: (SrcLocation,Path) -> PathList -> PathList 
ins (s   ,p   ) Empty = PathList s $ S.singleton p 
ins (sNew,pNew) (PathList s ps) 
  | s == sNew  = PathList s $ S.insert pNew ps
  | s > sNew   = singleton (sNew,pNew)
  | otherwise  = PathList s ps 

-- |Creates a PathList from a given list if pairs.
-- Thus, it returns the smallest source location and all paths to operations
-- in the tree annotated with this source location.
fromList :: [(SrcLocation,Path)] -> PathList
fromList = foldr ins Empty 

selectShortestPath :: PathList -> Maybe Path 
selectShortestPath Empty           = Nothing
selectShortestPath (PathList _ ps) = Just $ minimumBy (compare `on` length)  $ S.toList ps

-- |selects the smallest source location containing given cursor selection
smallestSrcLocContainingCursorPos :: CursorSelection -> Ast -> Maybe Path 
smallestSrcLocContainingCursorPos sele = 
 selectShortestPath . fromList . locsContainingSelection sele . findAllSrcLocations

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
