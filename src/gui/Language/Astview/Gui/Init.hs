{- provides 'setupGUI' the main gui initialization
function.
 -
 -}
module Language.Astview.Gui.Init(setupGui,setupAstState,hooks) where

import           Language.Astview.Gui.Actions
import           Language.Astview.Gui.GtkActions
import           Language.Astview.Gui.Menu
import           Language.Astview.Gui.Types
import           Language.Astview.Languages      (languages)

import           Control.Monad.Trans             (liftIO)
import           Data.IORef

import           Graphics.UI.Gtk                 hiding (Language)
import           Graphics.UI.Gtk.SourceView

setupAstState :: IO (IORef AstState)
setupAstState = do
  newIORef $ AstState (defaultValue { knownLanguages = languages}) defaultValue

-- |builds initial gui state from builder file
setupGui :: Builder -> IO GUI
setupGui builder = do
  win   <- builderGetObjectStr builder castToWindow "mainWindow"
  treeview <- builderGetObjectStr builder castToTreeView "treeview"
  tb <- buildSourceView =<< builderGetObjectStr builder castToScrolledWindow "swSource"
  return $ GUI win treeview tb

-- | setup the GtkSourceView and add it to the ScrollPane. return the
-- underlying textbuffer
buildSourceView :: ScrolledWindow -> IO SourceBuffer
buildSourceView sw = do
  sourceBuffer <- sourceBufferNew Nothing
  sourceBufferSetHighlightSyntax sourceBuffer True
  sourceView <- sourceViewNewWithBuffer sourceBuffer
  sourceViewSetShowLineNumbers sourceView True
  sourceViewSetHighlightCurrentLine sourceView True
  srcfont <- fontDescriptionFromString $ font defaultValue ++" "++show (fsize defaultValue)
  widgetModifyFont sourceView (Just srcfont)
  containerAdd sw sourceView
  return sourceBuffer

-- * hooks

-- | adds actions to widgets defined in type 'Gui'.
hooks :: AstAction (ConnectId Window)
hooks = do
  runner <- ioRunner
  storeLastActiveTextPosition runner
  storeLastActiveTreePosition runner
  closeAstviewOnWindowClosed runner
  close runner

type Hook a = (AstAction () -> IO ()) -> AstAction (ConnectId a)

-- |stores the last active cursor position in text to the program state
storeLastActiveTextPosition :: Hook SourceBuffer
storeLastActiveTextPosition runner = do
  buffer <- getSourceBuffer
  liftIO $ buffer `on` bufferChanged $ runner actionBufferChanged

-- |stores the path to the last selected tree cell to the program state
storeLastActiveTreePosition :: Hook TreeView
storeLastActiveTreePosition runner = do
  tree <- getTreeView
  liftIO $ tree `on` cursorChanged $ do
    (p,_) <- treeViewGetCursor tree
    runner (setTreePath p)

-- |softly terminate application on main window closed
closeAstviewOnWindowClosed :: Hook Window
closeAstviewOnWindowClosed runner = do
  w <- getWindow
  liftIO $ w `on` deleteEvent $ tryEvent $ liftIO $ runner actionQuit

-- |terminate application on main window closed
close :: Hook Window
close _ = do
  w <- getWindow
  liftIO $ w `on` objectDestroy $ mainQuit
