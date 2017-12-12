{- provides 'setupGUI' the main gui initialization
function.
 -
 -}
module Language.Astview.Gui.Init(setupGui,hooks) where

import           Language.Astview.Gui.Actions
import           Language.Astview.Gui.Menu
import           Language.Astview.Gui.Types
import           Language.Astview.Languages   (languages)

import           Control.Monad.Reader
import           Control.Monad.Trans          (liftIO)
import           Data.IORef

<<<<<<< 34651e0375c21059d8f26f0aea7afa4ce62f5f74
=======


>>>>>>> Hide IORef in Reader monad #11
import           Graphics.UI.Gtk              hiding (Language)
import           Graphics.UI.Gtk.SourceView

setupGui :: Builder -> IO (IORef AstState)
setupGui builder = do
  gui <- builderToGui builder
  let initState = AstState (defaultValue { knownLanguages = languages}) gui defaultValue
  newIORef initState

-- |builds initial gui state from builder file
builderToGui :: Builder -> IO GUI
builderToGui builder = do
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
  storeLastActiveTextPosition
  storeLastActiveTreePosition
  closeAstviewOnWindowClosed
  close

type Hook a = AstAction (ConnectId a)

-- |stores the last active cursor position in text to the program state
storeLastActiveTextPosition :: Hook SourceBuffer
storeLastActiveTextPosition = do
  ioref <- ask
  buffer <- getSourceBuffer

  liftIO $ buffer `on` bufferChanged $ do
    runReaderT actionBufferChanged ioref

-- |stores the path to the last selected tree cell to the program state
storeLastActiveTreePosition :: Hook TreeView
storeLastActiveTreePosition  = do
  ioref <- ask
  tree <- getTreeView

  liftIO $ tree `on` cursorChanged $ do
    (p,_) <- treeViewGetCursor tree
    runReaderT (setTreePath p) ioref

-- |softly terminate application on main window closed
closeAstviewOnWindowClosed :: Hook Window
closeAstviewOnWindowClosed = do
  ioref <- ask
  w <- getWindow
  liftIO $ w `on` deleteEvent $ tryEvent $ liftIO $ runReaderT actionQuit ioref

-- |terminate application on main window closed
close :: Hook Window
close = do
  w <- getWindow
  liftIO $ w `on` objectDestroy $ mainQuit
