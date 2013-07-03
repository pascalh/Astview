{-|
This module offers the main data type 'Language'. For every language, whose
files shall be processed by astview, a value of the data type 'Language' has
to be defined. Store the file to the data folder and add it to the cabal file.
-}
module Language.Astview.Language 
  ( Language(..)
  , SrcLocation(..)
  , AstNode(..)
  , Ast(..)
  , Error (..)
  )
where
import Data.Tree(Tree(..))
import Data.Generics (Typeable)

-- |specifies a source location in text area
data SrcLocation 
  -- | a single position defined by a cursor position
  = SrcPosition 
     Int -- ^ line, zero point: 1
     Int -- ^ row, zero point: 0
     
  -- |a span of selection, i.e. an interval of positions
  | SrcSpan
    Int -- ^begin line 
    Int -- ^begin row
    Int -- ^end line
    Int -- ^end row
    deriving (Eq,Typeable)

instance Show SrcLocation where
  show (SrcPosition l r) = show l ++ " : " ++ show r
  show (SrcSpan bl br el er) = show bl ++ " : " ++ show br ++ " , "++
                               show el ++ " : " ++ show er

instance Ord SrcLocation where
  s1 <= s2 = s2 `contains` s1 where
    contains :: SrcLocation -> SrcLocation -> Bool
    contains (SrcSpan bl br el er) (SrcPosition l r) = 
      (bl == el && l == el && br <= r && r <= er) || -- everything in one line
      (bl < l && l < el) ||
      (bl == l && br <= r && bl < el) ||
      (el== l && bl < el && r <=er)
    contains s (SrcSpan bl br el er) =
      contains s (SrcPosition bl br) && contains s (SrcPosition el er)
    contains _ _ = False
  s1 >= s2 = s2 <= s1
  s1 > s2 = s2 < s1

-- |a node represents an algebraic operation
data AstNode = AstNode
  { label :: String
  , srcloc :: Maybe SrcLocation
  }

instance Show AstNode where
  show (AstNode l s) = 
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

