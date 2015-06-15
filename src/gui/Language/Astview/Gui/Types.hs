{- contains the GUI data types
 -
 -}
module Language.Astview.Gui.Types where
import Data.Label
import Data.IORef

import Graphics.UI.Gtk hiding (Language,get,set)
import Graphics.UI.Gtk.SourceView (SourceBuffer)
import Language.Astview.Language(Language,SrcLocation(..),Path)

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

-- |data type for global options
data Options = Options
  { font :: String -- ^ font name of textbuffer
  , fsize :: Int -- ^ font size of textbuffer
  , flattenLists :: Bool -- ^should lists be flattened
  }

instance Default Options where
  defaultValue = Options "Monospace" 9 True

-- |data type for the intern program state
data State =  State
  { currentFile :: String -- ^ current file
  , textchanged :: Bool -- ^ true if file changed
  , lastSelectionInText :: SrcLocation -- ^ last active cursor position
  , lastSelectionInTree :: Path -- ^ last clicked tree cell
  , knownLanguages :: [Language] -- ^ known languages, which can be parsed
  }

instance Default State where
  defaultValue = State
        { currentFile = unsavedDoc
        , textchanged = False
        , lastSelectionInText  = SrcSpan 0 0 0 0
        , lastSelectionInTree = []
        , knownLanguages = []
        }

-- |unsaved document
unsavedDoc :: String
unsavedDoc = "Unsaved document"
-- |main gui data type, contains gtk components

data GUI = GUI
  { window :: Window -- ^ main window
  , tv :: TreeView -- ^ treeview
  , sb :: SourceBuffer -- ^ sourceview
  , mToggleFlatten :: CheckMenuItem -- ^ indicates whether trees should be flattened
  , dlgAbout :: AboutDialog -- ^ about dialog
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

getAboutDialog :: AstAction AboutDialog
getAboutDialog = fmap (dlgAbout . gui) . readIORef

getCheckMenuFlatten :: AstAction CheckMenuItem 
getCheckMenuFlatten = fmap (mToggleFlatten . gui) . readIORef

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

getCursor :: AstAction SrcLocation
getCursor = fmap (lastSelectionInText . state) . readIORef

getPath :: AstAction TreePath
getPath = fmap (lastSelectionInTree. state) . readIORef

getCurrentFile :: AstAction String
getCurrentFile = fmap (currentFile . state) . readIORef

getWindow :: AstAction Window
getWindow = fmap (window . gui) . readIORef

getFlattenLists :: AstAction Bool
getFlattenLists = fmap (flattenLists . options) . readIORef

-- * setter functions

lensSetIoRef :: (AstState :-> a) -> (a :-> b) -> b -> AstAction ()
lensSetIoRef outerLens innerLens value ref = modifyIORef ref m where

  m :: AstState -> AstState
  m = modify outerLens (set innerLens value)

-- |stores the given cursor selection
setCursor :: SrcLocation -> AstAction ()
setCursor cursor ref = lensSetIoRef lState lLastSelectionInText cursor ref

-- |stores the given tree selection
setTreePath :: Path -> AstAction ()
setTreePath path ref = lensSetIoRef lState lLastSelectionInTree path ref

-- |stores file path of current opened file
setCurrentFile :: FilePath -> AstAction ()
setCurrentFile file ref = lensSetIoRef lState lCurrentFile file ref

-- |stores whether the current file buffer has been changed
setChanged :: Bool -> AstAction ()
setChanged hasChanged ref = lensSetIoRef lState lTextchanged hasChanged ref

-- |stores whether the lists in trees should be flattened
setFlattenLists :: Bool -> AstAction () 
setFlattenLists flatten ref = lensSetIoRef lOptions lFlattenLists flatten ref
