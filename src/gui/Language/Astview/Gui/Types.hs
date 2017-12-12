{- contains the GUI data types
 -
 -}
module Language.Astview.Gui.Types where
import Data.Label
import Data.IORef
import Control.Monad.Reader

import Graphics.UI.Gtk hiding (Language,get,set)
import Graphics.UI.Gtk.SourceView (SourceBuffer)
import Language.Astview.Language(Language,SrcSpan,Path,position)

-- |a type class for default values, compareable to mempty in class 'Monoid'
class Default a where
  defaultValue :: a

type AstAction a = ReaderT (IORef AstState) IO  a

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


-- * getAstStateter functions

mkLabels [ ''AstState
         , ''Options
         , ''State
         , ''GUI
         ]



getAstState :: AstAction AstState
getAstState =  do
  ioRef <- ask
  liftIO (readIORef ioRef)

getSourceBuffer :: AstAction SourceBuffer
getSourceBuffer = (sb . gui) <$> getAstState

getTreeView :: AstAction TreeView
getTreeView = (tv . gui) <$> getAstState

getGui :: AstAction GUI
getGui = gui <$> getAstState

getState :: AstAction State
getState = state <$> getAstState

getKnownLanguages :: AstAction [Language]
getKnownLanguages = (knownLanguages . state) <$> getAstState

getChanged :: AstAction Bool
getChanged = (textchanged . state) <$> getAstState

getCursor :: AstAction SrcSpan
getCursor = (lastSelectionInText . state) <$> getAstState

getPath :: AstAction TreePath
getPath = (lastSelectionInTree . state) <$> getAstState


getCurrentFile :: AstAction String
getCurrentFile = (currentFile . state) <$> getAstState

getActiveLanguage :: AstAction (Maybe Language)
getActiveLanguage = (activeLanguage . state) <$> getAstState

getWindow :: AstAction Window
getWindow = (window . gui) <$> getAstState

getFlattenLists :: AstAction Bool
getFlattenLists = (flattenLists . options) <$> getAstState

getFontsize :: AstAction Int
getFontsize = (fsize . options) <$> getAstState

-- * setter functions

lensSetIoRef :: (AstState :-> a) -> (a :-> b) -> b -> AstAction ()
lensSetIoRef outerLens innerLens value = do
  ref <- ask
  liftIO $ modifyIORef ref m where

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
