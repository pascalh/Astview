{-# LANGUAGE Rank2Types #-}

{-|
this module contains a data type for source locations in a text buffer and
a couple of useful helper functions for source locations.
-}
module Language.Astview.SourceLocation
  (findSrcLoc
  ,pathToValues
  ,Path(Path)
  ,path
  ) where
import Data.List(find)
import Data.Data(Data)
import Data.Typeable(Typeable)
import qualified Data.Generics.Zipper as Z
import Data.Generics.Aliases(GenericQ)

import Language.Astview.Language
import Language.Astview.GUIData

selectionToSpan :: CursorSelection -> SrcLocation
selectionToSpan (CursorSelection lb rb le re) = SrcSpan lb rb le re

-- |given a cursor selection and a list of source locations, findSrcLoc determines 
-- the source location (from the list) which is nearest to the position given
-- by the line and the row.
findSrcLoc :: CursorSelection 
           -> [SrcLocation] 
           -> Maybe SrcLocation
findSrcLoc sele srcLocs = 
  sameElement sele srcLocs >> smallestSpanContaining sele srcLocs 

-- |returns the source position given by line and row, 
-- iff it is element of the list
sameElement :: CursorSelection -> [SrcLocation] -> Maybe SrcLocation
sameElement selection = 
  find (\s -> s==selectionToSpan selection)

-- |the source location which is the smallest one containing the position
-- given by line and row
smallestSpanContaining :: CursorSelection -> [SrcLocation] -> Maybe SrcLocation
smallestSpanContaining selection =
  smallestSpan . filter (\s -> contains s (selectionToSpan selection)) 

-- |returns the smallest span, i.e. the span with does not contain any other.
smallestSpan :: [SrcLocation] -> Maybe SrcLocation
smallestSpan []     = Nothing
smallestSpan (x:xs) = 
  Just $ foldr (\s acc -> if contains acc s then s else acc) x xs

-- |returns whether first argument source location contains the second one
contains :: SrcLocation -> SrcLocation -> Bool
contains (SrcSpan bl br el er) (SrcPosition l r) = 
  bl <= l && l<= el && br <= r && r <= er
contains s (SrcSpan bl br el er) =
  contains s (SrcPosition bl br) && contains s (SrcPosition el er)
contains _ _ = False

-- * extract source location of an arbitrary data type 

-- ** a zipper who knows his current position 

newtype Path = Path { path :: [Int] } deriving (Show,Eq,Ord)

moveRight :: Path -> Path
moveRight (Path p@(_:_)) = let l = last p in Path $ init p ++ [l+1]
moveRight (Path []) = 
  error "in moveRight: when moving to right in the tree no empty path allowed"

data Zipper a = Zipper (Z.Zipper a) Path 

toZipper :: Data a => a -> Zipper a
toZipper t = Zipper (Z.toZipper t) (Path [0])

-- |go down to leftmost child
down :: Zipper a -> Maybe (Zipper a)
down (Zipper z (Path p)) =
  case Z.down' z of  
    Nothing -> Nothing
    Just z' -> Just $ Zipper z' (Path (p++[0]))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper z p) =
  let p' = moveRight p in
  case Z.right z of  
    Nothing -> Nothing
    Just z' -> Just $ Zipper z' p'
   
-- ** map every term of an arbitrary type to its position in term

pathToValues :: (Typeable a,Data t) => (GenericQ (Maybe a)) -> t -> [(a,Path)]
pathToValues f = findPathsToValues f . toZipper 

findPathsToValues :: (Typeable a,Data t) 
                  => (GenericQ (Maybe a)) -> Zipper t -> [(a,Path)]
findPathsToValues f z = select f z ++ concatMap (findPathsToValues f) cs where
  cs = case down z of 
    Nothing -> [] 
    Just z'-> directSuccs z'

-- |returns zippers to all current successors of current zipper focus
directSuccs :: Zipper a -> [Zipper a]
directSuccs z = z : case right z of 
  Nothing -> [] 
  Just r -> directSuccs r 

-- |returns current focus with its position iff its type is @a@
select :: GenericQ (Maybe a) -> Zipper t -> [(a,Path)]
select f (Zipper z p) = 
  case Z.query f z of
    Nothing -> []
    Just r  -> [(r,p)]
