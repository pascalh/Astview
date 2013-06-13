{-# LANGUAGE TypeFamilies #-}

{-|
This module offers the main data type 'Language'. For every language, whose
files shall be processed by astview, a value of the data type 'Language' has
to be defined. Store the file to the data folder and add it to the cabal file.
-}
module Language.Astview.Language 
  ( Language
  , name , syntax, exts , parse, toSrcLoc
  , buildLanguage
  , Error (..)
  , SrcLocation(..)
  )
where
import Data.Generics (Data,Typeable)

-- |specifies a source location in text area
data SrcLocation 
  -- | a single position defined by a cursor position
  = SrcPosition 
     Int -- ^ line, zero point: 1
     Int -- ^ row, zero point: 0
     
  -- |a span of selection, i.e. an interval of positions
  | SrcSpan
    Int -- begin line 
    Int -- begin row
    Int -- end line
    Int -- end row
    deriving (Eq,Typeable)

instance Show SrcLocation where
  show (SrcPosition l r) = show l ++ " : " ++ show r
  show (SrcSpan bl br el er) = show bl ++ " : " ++ show br ++ " , "++
                               show el ++ " : " ++ show er


-- |datatype for one language. Some parsers support source locations
-- which enables us to connect locations in text area with locations
-- in a tree. Selector function @srcLoc@ supports extracting source
-- locations from a subtree (@srcLoc@ will be mapped over the whole
-- tree) to find source location of type @s@. 
data Language a s = Language
  { name :: String -- ^ language name
  , syntax :: String -- ^ syntax highlighter name
  , exts :: [String]
   -- ^ file extentions which should be associated with this language
  , parse :: String -> Either Error a -- ^ parse function
  , toSrcLoc :: Maybe (s -> SrcLocation) 
    -- ^source location extraction function
  } 

-- |more restrictive constructor for type Language
buildLanguage :: (Data a,Typeable s) 
         => String -> String -> [String] -> (String -> Either Error a) 
         -> Maybe (s -> SrcLocation) -> Language a s
buildLanguage = Language

instance Eq (Language a s) where
  l1 == l2 = name l1 == name l2

-- |datatype to specify parse errors. Since parsers offer different
-- amounts of information about parse errors, we offer the following
-- three parse errors: 
data Error
  = Err -- ^ no error information
  | ErrMessage String -- ^ simple error message
  | ErrLocation SrcLocation String -- ^ error message and src loc

