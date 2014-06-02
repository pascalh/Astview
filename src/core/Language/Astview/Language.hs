{-|
This module offers the main data type 'Language'. For every language, whose
files shall be processed by astview, a value of the data type 'Language' has
to be defined. 
-}
module Language.Astview.Language 
  ( Language(..)
  , SrcLocation(SrcSpan)
  , position
  , linear
  , CursorSelection(..)
  , selectionToSpan
  , NodeType(..)
  , AstNode(..)
  , Ast(..)
  , Error (..)
  )
where
import Data.Tree(Tree(..))
import Data.Generics (Typeable)
import Test.QuickCheck

-- |specifies a source location in text area. Use smart constructors 'linear'
-- and 'position' to create special source locations.
data SrcLocation 
  =  SrcSpan
    Int -- ^begin line 
    Int -- ^begin row
    Int -- ^end line
    Int -- ^end row
    deriving (Eq,Typeable)

instance Show SrcLocation where
  show (SrcSpan bl br el er) = show bl ++ " : " ++ show br ++ " , "++
                               show el ++ " : " ++ show er

instance Ord SrcLocation where
  s1 >= s2 = s2 <= s1
  s1 > s2 = s2 < s1
  (SrcSpan bl br el er) <= s2 =
    s2 `contains` (bl,br) && s2 `contains` (el,er)  

-- |returns whether the given source location contains the position pair
-- defined by line and row.
contains :: SrcLocation -> (Int,Int)-> Bool
contains (SrcSpan br bc er ec) (r , c) = 
  (br == er && r == er && bc <= c && c <= ec) || 
  (br < r && r < er) ||
  (br == r && bc <= c && br < er) ||
  (er == r && br < er && c <= ec)

instance Arbitrary SrcLocation where
  arbitrary =  do 
    (NonNegative i1) <- arbitrary 
    (NonNegative i2) <- arbitrary 
    (NonNegative i3) <- arbitrary 
    (NonNegative i4) <- arbitrary 
    return $ SrcSpan i1 i2 (i1+i3) (i2+i4)

-- |a smart constructor for 'SrcLocation' to define exact positions 
position :: Int -> Int -> SrcLocation
position line row = SrcSpan line row line row

-- |a smart constructor for 'SrcLocation' to define spans which range
-- over only one specific line and more than one row.
linear :: Int -- ^ the line
     -> Int  -- ^ begin row
     -> Int  -- ^ end row
     -> SrcLocation
linear line beginRow = SrcSpan line beginRow line 

-- |a cursor selection in a text buffer
data CursorSelection = CursorSelection
  Int -- ^ line start
  Int  -- ^ row start
  Int -- ^ line end
  Int  -- ^ row end

instance Show CursorSelection where
  show (CursorSelection a b c d) = 
    "("++show a++":"++show b++") ("++show c++":"++show d++")"

-- |transforms a CursorSelection to a SrcSpan
selectionToSpan :: CursorSelection -> SrcLocation
selectionToSpan (CursorSelection lb rb le re) = SrcSpan lb rb le re

-- |a node represents either an operation or an identificator
data NodeType = Operation | Identificator

-- |a node represents an algebraic operation
data AstNode = AstNode
  { label :: String
  , srcloc :: Maybe SrcLocation
  , path :: [Int]
  , nodeType :: NodeType
  }

instance Show AstNode where
  show (AstNode l s _ _) = 
    l ++ (case s of { Nothing -> ""; 
                      Just x ->replicate 5 ' '  ++"["++show x++"]"})

-- |an (untyped) abstract syntax is just a tree of AstNodes
newtype Ast = Ast (Tree AstNode) 

-- |datatype for one language. Some parsers support source locations
-- which enables us to connect locations in text area with locations
-- in a tree. Selector function @srcLoc@ supports extracting source
-- locations from a subtree (@srcLoc@ will be mapped over the whole
-- tree) to find source location of type @s@. 
data Language = Language
  { name :: String -- ^ language name
  , syntax :: String -- ^ syntax highlighter name
  , exts :: [String]
   -- ^ file extentions which should be associated with this language
  , parse :: String -> Either Error Ast -- ^ parse function
  } 

instance Eq Language where
  l1 == l2 = name l1 == name l2

-- |datatype to specify parse errors. Since parsers offer different
-- amounts of information about parse errors, we offer the following
-- three parse errors: 
data Error
  = Err -- ^ no error information
  | ErrMessage String -- ^ simple error message
  | ErrLocation SrcLocation String -- ^ error message and src loc

