{-|
this module contains a data type for source locations in a text buffer and
a couple of useful helper functions for source locations.
-}
module Language.Astview.SourceLocation where
import Data.List(find)
import Data.Monoid

-- |specifies a source location in text area
data SrcLocation 
  -- | a single position defined by a cursor position
  = SrcPosition 
    { line :: Int -- ^ line, zero point: 1
    , row  :: Int -- ^ row, zero point: 0
    } 
  -- |a span of selection, i.e. an interval of positions
  | SrcSpan
    { beginLine :: Int 
    , beginRow  :: Int
    , endLine   :: Int
    , endRow    :: Int
    }
    deriving Eq

instance Show SrcLocation where
  show (SrcPosition l r) = show l ++ " : " ++ show r
  show (SrcSpan bl br el er) = show bl ++ " : " ++ show br ++ ","++
                               show el ++ " : " ++ show er

-- |given a line, a row and a list of source locations, findSrcLoc determines 
-- the source location (from the list) which is nearest to the position given
-- by the line and the row.
findSrcLoc :: Int -- ^line
           -> Int -- ^row 
           -> [SrcLocation] 
           -> Maybe SrcLocation
findSrcLoc l r srcLocs
  | elem (SrcPosition l r) srcLocs = Just (SrcPosition l r) 
  | otherwise        = 
   getFirst $ (First $ find (\s -> contains s (SrcPosition l r)) srcLocs) `mappend` 
              (First $ find equalsLine srcLocs)  where
    equalsLine :: SrcLocation -> Bool
    equalsLine (SrcPosition ll _) = ll ==l
    equalsLine _ = False 

-- |returns the value which is associated with given source location
getAssociatedValue :: Maybe SrcLocation -> [(SrcLocation,[a])] -> [a] 
getAssociatedValue Nothing _ = []
getAssociatedValue _ [] = []
getAssociatedValue (Just s) ((loc,v):xs)
  | s == loc  = v
  | otherwise = getAssociatedValue (Just s) xs

-- |returns whether first argument source location contains the second one
contains :: SrcLocation -> SrcLocation -> Bool
contains (SrcSpan bl br el er) (SrcPosition l r) = 
  bl <= l && l<= el && br <= r && r <= er
contains s (SrcSpan bl br el er) =
  contains s (SrcPosition bl br) && contains s (SrcPosition el er)
contains _ _ = False
