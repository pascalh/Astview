{- contains the GUI data types
 -
 -}
module Language.Astview.Gui.Types where
import Data.Label
import Data.IORef

-- gtksourceview
import Graphics.UI.Gtk hiding (Language,get,set)
import Graphics.UI.Gtk.SourceView (SourceBuffer) 
import Language.Astview.Language(Language,CursorSelection(..))

-- |a type class for default values, compareable to mempty in class 'Monoid'
class Default a where
  defaultVaule :: a

type AstAction a = IORef AstState -> IO a

-- |union of intern program state and gui
data AstState = AstState
  { state :: State -- ^ intern program state
  , gui :: GUI -- ^ gtk data types
  , options :: Options -- ^ global program options
  }

-- |data type for global options
data Options = Options
  { font :: String -- ^ font name of textbuffer
  , fsize :: Int -- ^ font size of textbuffer
  }

instance Default Options where
  defaultVaule = Options "Monospace" 9

-- |data type for the intern program state
data State =  State
  { cFile :: String -- ^ current file
  , textchanged :: Bool -- ^ true if file changed
  , lastSelectionInText :: CursorSelection -- ^ last active cursor position
  , lastSelectionInTree :: TreePath -- ^ last clicked tree cell
  , languages :: [Language] -- ^ known languages
  }

instance Default State where
  defaultVaule = State 
        { cFile = unsavedDoc
        , textchanged = False
        , lastSelectionInText  = CursorSelection 0 0 0 0
        , lastSelectionInTree = []
        , languages = [] 
        }

-- |unsaved document
unsavedDoc :: String
unsavedDoc = "Unsaved document"
-- |main gui data type, contains gtk components

data GUI = GUI
  { window :: Window -- ^ main window
  , tv :: TreeView -- ^ treeview
  , sb :: SourceBuffer -- ^ sourceview
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

getAstState :: AstAction AstState
getAstState = readIORef

getGui :: AstAction GUI
getGui = fmap gui . readIORef 

getState :: AstAction State 
getState = fmap state . readIORef 

getLangs :: AstAction [Language]
getLangs = fmap (languages . state) . readIORef

getChanged :: AstAction Bool
getChanged = fmap (textchanged . state) . readIORef

getCursor :: AstAction CursorSelection
getCursor = fmap (lastSelectionInText . state) . readIORef

getPath :: AstAction TreePath
getPath = fmap (lastSelectionInTree. state) . readIORef

getFile :: AstAction String
getFile = fmap (cFile . state) . readIORef

getWindow :: AstAction Window
getWindow = fmap (window . gui) . readIORef

-- * setter functions

lensSetIoRef :: (AstState :-> a) -> (a :-> b) -> b -> AstAction ()
lensSetIoRef outerLens innerLens value ref = modifyIORef ref m where

  m :: AstState -> AstState
  m = modify outerLens (set innerLens value)

-- |stores the given cursor selection 
setCursor :: CursorSelection -> AstAction ()
setCursor cursor ref = lensSetIoRef lState lLastSelectionInText cursor ref

-- |stores the given tree selection 
setTreePath :: TreePath -> AstAction ()
setTreePath path ref = lensSetIoRef lState lLastSelectionInTree path ref

-- |stores file path of current opened file 
setcFile :: FilePath -> AstAction ()
setcFile file ref = lensSetIoRef lState lCFile file ref

-- |stores whether the current file buffer has been changed
setChanged :: Bool -> AstAction ()
setChanged changed ref = lensSetIoRef lState lTextchanged changed ref

