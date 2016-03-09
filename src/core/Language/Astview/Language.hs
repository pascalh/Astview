{-|
This module offers the main data type 'Language'. A value of 'Language'
states how their files shall be processed by astview.
-}
module Language.Astview.Language
  ( Language(..)
  , SrcSpan(..)
  , SrcPos(..)
  , position
  , linear
  , span
  , NodeType(..)
  , Path
  , AstNode(..)
  , Ast(..)
  , Error (..)
  , SrcLocException(..)
  )
where
import Prelude hiding (span)
import Data.Tree(Tree(..))
import Data.Generics (Typeable,Data)
import Test.QuickCheck
import Control.Exception(Exception,throw)

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
-- The file extentions of all languages known to astview may overlap, since
-- a language can be manually selected in the menu. For perfect automatic
-- language detection the extentions need to be disjoint.
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

-- |represents a source position.
data SrcPos = SrcPos { line :: Int , column :: Int } deriving (Eq,Ord,Typeable,Data)

instance Show SrcPos where
  show (SrcPos l c) = show l ++ " : "++show c

instance Arbitrary SrcPos where
  arbitrary = do
    NonNegative l <- arbitrary
    NonNegative c <- arbitrary
    return $ SrcPos l c

-- |specifies a source span in a text area consisting of a begin position
-- and a end position.
-- Use functions 'span', 'linear' and 'position' to create source spans, since
-- they apply validity checks.
data SrcSpan =  SrcSpan { begin :: SrcPos , end :: SrcPos }
  deriving (Eq,Typeable,Data)

instance Show SrcSpan where
  show (SrcSpan b e) = show b  ++ " , " ++ show e

instance Ord SrcSpan where
  s1 >= s2 = s2 <= s1
  s1 > s2 = s2 < s1
  (SrcSpan b e) <= s2 = s2 `contains` b && s2 `contains` e

-- |returns whether the given source span contains the position
contains :: SrcSpan -> SrcPos -> Bool
contains (SrcSpan (SrcPos br bc) (SrcPos er ec)) (SrcPos r c) =
  let s = (r,c) in (br,bc) <= s && s <= (er,ec)

instance Arbitrary SrcSpan where
  arbitrary =  do
    pos@(SrcPos l c ) <- arbitrary
    (NonNegative l') <- arbitrary
    (NonNegative c') <- arbitrary
    return $ SrcSpan pos $ SrcPos (l+l') (c+c')

-- |a smart constructor for 'SrcSpan', which also applies validity checks.
--
-- >>> span 1 2 3 4
-- SrcSpan (SrcPos 1 2) (SrcPos 3 4))
span :: Int -> Int -> Int -> Int -> SrcSpan
span bl bc el ec
  | bl > el           = throw $ SrcLocException s
  | bl == el && bc > ec = throw $ SrcLocException s
  | otherwise = s where
    s = SrcSpan (SrcPos bl bc) (SrcPos el ec)

-- |a constructor for 'SrcSpan' to define an exact position.
--
-- >>> position 1 2
-- SrcSpan (SrcPos 1 2) (SrcPos 1 2))
position :: Int -- ^line
         -> Int -- ^row
         -> SrcSpan
position line row = let p = SrcPos line row in SrcSpan p p

-- |a constructor for 'SrcSpan' to define a span which ranges
-- over one specific line and more than one row. Since 'linear'
-- is implemented using 'span' validity of input is being checked.
--
-- >>> linear 1 3 12
-- SrcSpan (SrcPos 1 3) (SrcPos 1 12))
linear :: Int -- ^ line
     -> Int  -- ^ begin row
     -> Int  -- ^ end row
     -> SrcSpan
linear line beginRow endRow = span line beginRow line endRow

data SrcLocException = SrcLocException SrcSpan deriving (Eq)

instance Show SrcLocException where
  show (SrcLocException s) = "Source location "++show s++" is not valid."

instance Exception SrcLocException
