{-|
This module offers the main data type 'Language'. For every language, whose
files shall be processed by astview, a value of the data type 'Language' has
to be defined. Store the file to the data folder and add it to the cabal file.
-}
module Language.Astview.Language where
import Data.Generics (Typeable)
import Data.Tree (Tree(..))
import Language.Astview.SourceLocation

-- |datatype for one language. Some parsers support source locations
-- which enables us to connect locations in text area with locations
-- in a tree. Selector function @srcLoc@ supports extracting source
-- locations from a subtree (@srcLoc@ will be mapped over the whole
-- tree). @srcLoc@ returns @Nothing@ if current tree does not specify
-- any src loc. Function @adjustSrcLoc@ offers the ability to adjust
-- src locs in abstract data type to our zero point (line 1, row 0)
data Language = forall a . Language
  { name :: String -- ^ language name
  , syntax :: String -- ^ syntax highlighter name
  , exts :: [String] 
   -- ^ file extentions which should be associated with this language
  , parse :: String -> Either Error a -- ^ parse function
  , toTree :: a -> Tree String -- ^ how to get a 'Tree' 'String'?
  , srcLoc :: Tree String -> Maybe SrcLocation
    -- ^ selector function for source locations (if supported)
  } deriving Typeable

instance Eq Language where
  l1 == l2 = name l1 == name l2


-- |datatype to specify parse errors. Since parsers offer different
-- amounts of information about parse errors, we offer the following
-- three parse errors: 
data Error
  = Err -- ^ no error information
  | ErrMessage String -- ^ simple error message
  | ErrLocation SrcLocation String -- ^ error message and src loc

