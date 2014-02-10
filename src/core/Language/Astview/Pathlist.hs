{- |
One source location can be represented by multiple nodes in a abstract
syntax tree.  A pathlist stores a source location and the associated tree 
positions. 

This data structure is specialized to find the smallest source location
surrounded by a given cursor position in module 
'Language.Astview.SourceLocation'. Thus, if we find a smaller source 
location than the current during insertion , the current one is replaced 
by the new. See function 'ins' for details.

-}
module Language.Astview.Pathlist
  (PathList
  ,toList
  ,ins
  ,fromList
  ) where
import Data.List(sort)
import Language.Astview.Language (SrcLocation(..))

-- |a pathlist stores a source location and paths to all subtrees annotated
-- with the source location
data PathList 
  = Empty -- ^ the empty list
  | PathList  SrcLocation [[Int]] 
    -- ^ a source location and their associated tree positions

instance Eq PathList where
  Empty             == Empty             = True
  (PathList s1 ps1) == (PathList s2 ps2) = s1 == s2 && sort ps1 == sort ps2
  _                 == _                 = False

instance Show PathList where
  show Empty = "<>"
  show (PathList s p) = "<"++show s++" @ "++show p++">"

singleton :: (SrcLocation,[Int]) -> PathList
singleton (x,p) = ins (x,p) Empty 

ins :: (SrcLocation,[Int]) -> PathList -> PathList 
ins (s   ,p   ) Empty = PathList s [p] 
ins (sNew,pNew) (PathList s ps) 
  | s == sNew  = PathList s $ pNew:ps
  | s > sNew   = singleton (sNew,pNew)
  | otherwise  = PathList s ps 

toList :: PathList -> [(SrcLocation,[Int])]
toList Empty           = []
toList (PathList s ps) = map (\p -> (s,p)) ps

-- |Creates a PathList from a given list if pairs.
-- Thus, it returns the smallest source location and all paths to operations
-- in the tree annotated with this source location.
fromList :: [(SrcLocation,[Int])] -> PathList
fromList = foldr ins Empty 
