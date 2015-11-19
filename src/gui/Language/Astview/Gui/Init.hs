{- provides 'setupGUI' the main gui initialization
function. (using the module Language.Astview.Menu to build the menu bar)
 -
 -}
module Language.Astview.Gui.Init(setupGUI) where

import Language.Astview.Gui.Types
import Language.Astview.Gui.Actions
import Language.Astview.Gui.Menu
import Language.Astview.Languages(languages)

import Control.Monad.Trans (liftIO)
import Data.IORef
import System.FilePath ((</>))

import Graphics.UI.Gtk hiding (Language)
import Graphics.UI.Gtk.SourceView
import Paths_astview (getDataFileName)

-- |builds initial gui state from builder file
builderToGui :: Builder -> IO GUI
builderToGui builder = do
  win   <- builderGetObjectStr builder castToWindow "mainWindow"
  treeview <- builderGetObjectStr builder castToTreeView "treeview"
  tb <- buildSourceView =<< builderGetObjectStr builder castToScrolledWindow "swSource"
  return $ GUI win treeview tb

-- |creates initial program state and provides an IORef to that
buildState :: Builder -> IO (IORef AstState)
buildState builder = do
  g <- builderToGui builder
  let astSt = AstState st g defaultValue
      st = defaultValue { knownLanguages = languages}
  newIORef astSt

-- | initiates gui and returns initial program state
setupGUI :: IO (IORef AstState)
setupGUI = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName ("data" </> "astview.xml")
  r <- buildState builder
  initMenu builder r
  hooks r
  return r

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

-- ** hooks

-- | adds actions to widgets defined in type 'Gui'. (see 'hookNonGuiStateWidgets')
hooks :: AstAction (ConnectId Window)
hooks ref = do
  textbuffer <- getSourceBuffer ref
  storeLastActiveTextPosition textbuffer ref

  tree <- getTreeView ref
  storeLastActiveTreePosition tree ref

  win <- getWindow ref
  closeAstviewOnWindowClosed win ref
  close win ref

type Hook a = a -> AstAction (ConnectId a)

-- |stores the last active cursor position in text to the program state
storeLastActiveTextPosition :: Hook SourceBuffer
storeLastActiveTextPosition buffer ref = buffer `on` bufferChanged $ do
    actionBufferChanged ref
    cp <- getCursorPosition ref
    setCursor cp ref

-- |stores the path to the last selected tree cell to the program state
storeLastActiveTreePosition :: Hook TreeView
storeLastActiveTreePosition tree ref =
  tree `on` cursorChanged   $ do
    (p,_) <- treeViewGetCursor tree
    setTreePath p ref

-- |softly terminate application on main window closed
closeAstviewOnWindowClosed :: Hook Window
closeAstviewOnWindowClosed w ref =
  w `on` deleteEvent $ tryEvent $ liftIO $ actionQuit ref

-- |terminate application on main window closed
close :: Hook Window
close w _ =  w `on` objectDestroy $ mainQuit
