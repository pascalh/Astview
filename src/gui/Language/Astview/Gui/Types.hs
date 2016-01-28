{- contains the GUI data types
 -
 -}
module Language.Astview.Gui.Types where
import Data.Label
import Data.IORef

import Graphics.UI.Gtk hiding (Language,get,set)
import Graphics.UI.Gtk.SourceView (SourceBuffer)
import Language.Astview.Language(Language,SrcSpan,Path,position)

-- |a type class for default values, compareable to mempty in class 'Monoid'
class Default a where
  defaultValue :: a

type AstAction a = IORef AstState -> IO a

-- |union of internal program state and gui
data AstState = AstState
  { state :: State -- ^ intern program state
  , gui :: GUI -- ^ gtk data types
  , options :: Options -- ^ global program options
  }

-- |data type for global options, which can be directly changed in the gui
-- (...or at least should be. Menu for changing font and font size not yet
-- implemented)
data Options = Options
  { font :: String -- ^ font name of textbuffer
  , fsize :: Int -- ^ font size of textbuffer
  , flattenLists :: Bool -- ^should lists be flattened
  }

instance Default Options where
  defaultValue = Options "Monospace" 9 True

-- |data type for the internal program state
data State =  State
  { currentFile :: String -- ^ current file
  , textchanged :: Bool -- ^ true if buffer changed after last save
  , lastSelectionInText :: SrcSpan -- ^ last active cursor position
  , lastSelectionInTree :: Path -- ^ last clicked tree cell
  , knownLanguages :: [Language] -- ^ known languages, which can be parsed
  , activeLanguage :: Maybe Language -- ^the currently selected language or Nothing if language is selected by file extension
  }

instance Default State where
  defaultValue = State
        { currentFile = unsavedDoc
        , textchanged = False
        , lastSelectionInText = position 0 0
        , lastSelectionInTree = []
        , knownLanguages = []
        , activeLanguage = Nothing
        }

-- |unsaved document
unsavedDoc :: String
unsavedDoc = "Unsaved document"
-- |main gui data type, contains gtk components

data GUI = GUI
  { window :: Window -- ^ main window
  , tv :: TreeView -- ^ treeview
  , sb :: SourceBuffer -- ^ sourceview
  }


-- * getter functions

mkLabels [ ''AstState
         , ''Options
         , ''State
         , ''GUI
         ]

getSourceBuffer :: AstAction SourceBuffer
getSourceBuffer = fmap (sb . gui) . readIORef

getTreeView :: AstAction TreeView
getTreeView = fmap (tv . gui) . readIORef

getAstState :: AstAction AstState
getAstState = readIORef

getGui :: AstAction GUI
getGui = fmap gui . readIORef

getState :: AstAction State
getState = fmap state . readIORef

getKnownLanguages :: AstAction [Language]
getKnownLanguages = fmap (knownLanguages . state) . readIORef

getChanged :: AstAction Bool
getChanged = fmap (textchanged . state) . readIORef

getCursor :: AstAction SrcSpan
getCursor = fmap (lastSelectionInText . state) . readIORef

getPath :: AstAction TreePath
getPath = fmap (lastSelectionInTree. state) . readIORef

getCurrentFile :: AstAction String
getCurrentFile = fmap (currentFile . state) . readIORef

getActiveLanguage :: AstAction (Maybe Language)
getActiveLanguage = fmap (activeLanguage . state) . readIORef

getWindow :: AstAction Window
getWindow = fmap (window . gui) . readIORef

getFlattenLists :: AstAction Bool
getFlattenLists = fmap (flattenLists . options) . readIORef

getFontsize :: AstAction Int
getFontsize = fmap (fsize . options) . readIORef

-- * setter functions

lensSetIoRef :: (AstState :-> a) -> (a :-> b) -> b -> AstAction ()
lensSetIoRef outerLens innerLens value ref = modifyIORef ref m where

  m :: AstState -> AstState
  m = modify outerLens (set innerLens value)

-- |stores the given cursor selection
setCursor :: SrcSpan -> AstAction ()
setCursor = lensSetIoRef lState lLastSelectionInText

-- |stores the given tree selection
setTreePath :: Path -> AstAction ()
setTreePath = lensSetIoRef lState lLastSelectionInTree

-- |stores file path of current opened file
setCurrentFile :: FilePath -> AstAction ()
setCurrentFile = lensSetIoRef lState lCurrentFile

-- |stores whether the current file buffer has been changed
setChanged :: Bool -> AstAction ()
setChanged = lensSetIoRef lState lTextchanged

-- |stores whether the lists in trees should be flattened
setFlattenLists :: Bool -> AstAction ()
setFlattenLists = lensSetIoRef lOptions lFlattenLists

setActiveLanguage :: Maybe Language -> AstAction ()
setActiveLanguage = lensSetIoRef lState lActiveLanguage
