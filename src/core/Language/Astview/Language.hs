{-|
This module offers the main data type 'Language'. A value of 'Language'
states how their files shall be processed by astview.
-}
module Language.Astview.Language
  ( Language(..)
  , SrcSpan(SrcSpan)
  , position
  , linear
  , NodeType(..)
  , Path
  , AstNode(..)
  , Ast(..)
  , Error (..)
  )
where
import Data.Tree(Tree(..))
import Data.Generics (Typeable)
import Test.QuickCheck

-- |'NodeType' distinguishes two kinds of nodes in arbitrary haskell terms:
--
--    * (leaf) nodes representing an identificator (and thus a 'String').
--      Note: usually strings are lists of characters and therefore subtrees
--      of an abstract syntax tree, but we flatten these subtrees to one
--      node, which will then be annotated with 'Identificator'.
--
--    * all constructors in a term not representing an identificator are just
--      an 'Operation'
--
data NodeType = Operation
              | Identificator
              deriving Eq

-- |A position in a tree is uniquely determined by a list of natural numbers.
-- (beginning with @0@).
type Path = [Int]

-- |'AstNode' represents a node in an untyped abstract syntax tree
-- annotated with additional information.
data AstNode = AstNode
  { label :: String -- ^ constructor name or the representing string
  , srcspan :: Maybe SrcSpan -- ^the source span this node represents in the parsed text (if existing)
  , path :: Path -- ^ the path from the root of the tree to this node
  , nodeType :: NodeType -- ^the node type
  }
  deriving Eq

instance Show AstNode where
  show (AstNode l s _ _) =
    l ++ (case s of { Nothing -> "";
                      Just x ->replicate 5 ' '  ++"["++show x++"]"})

-- |An (untyped) abstract syntax tree is just a tree of 'AstNode's.
newtype Ast = Ast { ast :: Tree AstNode }

-- |A value of 'Language' states how files (associated with this language by
-- their file extentions 'exts') are being parsed.
-- The file extentions of all languages known to astview may not overlap, since
-- a language is selected by the extention of the currently opened file.
-- A future release of astview should support manual language selection and thus
-- making the restriction obsolete.
data Language = Language
  { name :: String -- ^ language name
  , syntax :: String
  -- ^ (kate) syntax highlighter name. Use @[]@ if no highlighting is desired.
  , exts :: [String]
   -- ^ file extentions which should be associated with this language
  , parse :: String -> Either Error Ast -- ^ parse function
  }

-- |Since parsers return different
-- amounts of information about parse errors, we distinguish the following
-- three kinds of parse errors:
data Error
  = Err -- ^ no specific error information
  | ErrMessage String -- ^ plain error message
  | ErrLocation SrcSpan String -- ^ error message with position information

-- * source locations and spans

-- |specifies a source span in a text area consisting of a begin row, begin
-- column, end row and end column.
--Use 'linear' and 'position' to create special source spans.
-- Both functions do not check validity of source spans, since we
-- assume that parsers return valid data.
data SrcSpan =  SrcSpan Int Int Int Int deriving (Eq,Typeable)

instance Show SrcSpan where
  show (SrcSpan bl br el er) = show bl ++ " : " ++ show br ++ " , "++
                               show el ++ " : " ++ show er

instance Ord SrcSpan where
  s1 >= s2 = s2 <= s1
  s1 > s2 = s2 < s1
  (SrcSpan bl br el er) <= s2 =
    s2 `contains` (bl,br) && s2 `contains` (el,er)

-- |returns whether the given source span contains the position pair
-- defined by line and row.
contains :: SrcSpan -> (Int,Int)-> Bool
contains (SrcSpan br bc er ec) (r , c) =
  (br == er && r == er && bc <= c && c <= ec) ||
  (br < r && r < er) ||
  (br == r && bc <= c && br < er) ||
  (er == r && br < er && c <= ec)

instance Arbitrary SrcSpan where
  arbitrary =  do
    (NonNegative i1) <- arbitrary
    (NonNegative i2) <- arbitrary
    (NonNegative i3) <- arbitrary
    (NonNegative i4) <- arbitrary
    return $ SrcSpan i1 i2 (i1+i3) (i2+i4)

-- |a constructor for 'SrcSpan' to define an exact position.
position :: Int -- ^line
         -> Int -- ^row
         -> SrcSpan
position line row = SrcSpan line row line row

-- |a constructor for 'SrcSpan' to define a span which ranges
-- over one specific line and more than one row.
linear :: Int -- ^ line
     -> Int  -- ^ begin row
     -> Int  -- ^ end row
     -> SrcSpan
linear line beginRow = SrcSpan line beginRow line

