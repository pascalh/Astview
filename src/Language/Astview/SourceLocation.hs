{-|
this module contains a data type for source locations in a text buffer and
a couple of useful helper functions for source locations.
-}
module Language.Astview.SourceLocation
  (select
  ,PathList
  ,toList
  ,singleton
  ,ins
  )

where
import Data.Maybe(catMaybes)
import Data.Tree(Tree(..),flatten)
import Data.List(sort)

import Language.Astview.Language
import Language.Astview.GUIData(CursorSelection(..))

-- |transforms a CursorSelection to a SrcSpan
selectionToSpan :: CursorSelection -> SrcLocation
selectionToSpan (CursorSelection lb rb le re) = SrcSpan lb rb le re

-- |select the source location path pairs in the tree, s.t.
-- the source locations are the smallest containing given cursor selection.
-- Thus the pairs in the resulting list only differ in their paths.
select :: CursorSelection -> Ast -> PathList
select sele (Ast ast) = bruteForce ast where
  
    bruteForce :: Tree AstNode -> PathList
    bruteForce tree = 
      let allSrcLocs = catMaybes $ flatten $ fmap getSrcLocPathPairs tree
      in smallest $ locsContainingSelection allSrcLocs 

    getSrcLocPathPairs :: AstNode -> Maybe (SrcLocation,[Int])
    getSrcLocPathPairs (AstNode _ Nothing  _ _) = Nothing
    getSrcLocPathPairs (AstNode _ (Just s) p _) = Just (s,p)

    locsContainingSelection :: [(SrcLocation,[Int])] -> [(SrcLocation,[Int])] 
    locsContainingSelection = filter (\(s,_) -> s >= selectionToSpan sele )

-- * 

-- |a pathlist stores a source location and paths to all subtrees annotated
-- with the source location
data PathList 
  = Empty -- ^ the empty list
  | PathList  SrcLocation [[Int]] -- ^ a source location and positions in the tree
                                -- where this location occurs

instance Eq PathList where
  Empty             == Empty             = True
  (PathList s1 ps1) == (PathList s2 ps2) = s1 == s2 && (sort ps1) == (sort ps2)
  _ == _ = False

instance Show PathList where
  show Empty = "<>"
  show (PathList s p) = "<"++show s++" @ "++show p++">"

singleton :: (SrcLocation,[Int]) -> PathList
singleton (x,p) = PathList x [p]

ins :: (SrcLocation,[Int]) -> PathList -> PathList 
ins (s   ,p   ) Empty = PathList s [p] 
ins (sNew,pNew) (PathList s ps) 
  | s == sNew  = PathList s $ pNew:ps
  | s > sNew   = singleton (sNew,pNew)
  |otherwise   = PathList s ps 

toList :: PathList -> [(SrcLocation,[Int])]
toList Empty = []
toList (PathList s ps) = map (\p -> (s,p)) ps

-- |returns the smallest source location and all paths to operations
-- annotated with this location.
smallest :: [(SrcLocation,[Int])] -> PathList
smallest []     = Empty 
smallest (x:xs) = ins x (smallest xs)
