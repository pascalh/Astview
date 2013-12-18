{- contains the GUI data types
 -
 -}
module Language.Astview.GUIData where

import Data.IORef

-- gtksourceview
import Graphics.UI.Gtk hiding (Language,get)
import Graphics.UI.Gtk.SourceView (SourceBuffer) 
import Language.Astview.Language(Language,CursorSelection(..))

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

-- |data type for the intern program state
data State =  State
  { cFile :: String -- ^ current file
  , textchanged :: Bool -- ^ true if file changed
  , lastSelection :: CursorSelection -- ^ last active cursor position
  , languages :: [Language] -- ^ known languages
  }

-- |main gui data type, contains gtk components
data GUI = GUI
  { window :: Window -- ^ main window
  , tv :: TreeView -- ^ treeview
  , sb :: SourceBuffer -- ^ sourceview
  , dlgAbout :: AboutDialog -- ^ about dialog
  }


-- * getter functions

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

getLangs :: AstAction [Language]
getLangs = fmap (languages . state) . readIORef

getChanged :: AstAction Bool
getChanged = fmap (textchanged . state) . readIORef

getCursor :: AstAction CursorSelection
getCursor = fmap (lastSelection . state) . readIORef

getFile :: AstAction String
getFile = fmap (cFile . state) . readIORef

getWindow :: AstAction Window
getWindow = fmap (window . gui) . readIORef

-- * setter functions

-- |stores the current selection 
setCursor :: CursorSelection -> AstAction ()
setCursor cp r = modifyIORef r m  where
  m :: AstState -> AstState
  m s@(AstState (State f c _ ls ) _ _) = s { state = State f c cp ls}

-- |stores file path of current opened file 
setcFile :: FilePath -> AstAction ()
setcFile file r = modifyIORef r m where
  m :: AstState -> AstState
  m s@(AstState (State _ cp c ls) _ _) = s { state = State file cp c ls}

-- |stores whether the current file buffer has been changed
setChanged :: Bool -> AstAction ()
setChanged b r = modifyIORef r m where
  m :: AstState -> AstState
  m s@(AstState (State file _ c ls) _ _) = s { state = State file b c ls}
