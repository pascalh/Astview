{- provides 'setupGUI' the main gui initialization
function
 -
 -}
module Language.Astview.Gui.Init(setupGUI) where

-- guiactions
import Language.Astview.Gui.Types
import Language.Astview.Gui.Actions

-- base
import Control.Monad.Trans (liftIO)
import Data.IORef
-- filepath
import System.FilePath ((</>))

-- gtk
import Graphics.UI.Gtk hiding (Language)

-- glade
import Graphics.UI.Gtk.Glade

-- gtksourceview
import Graphics.UI.Gtk.SourceView

-- astview-utils
import Language.Astview.Languages(languages)

-- generated on-the-fly by cabal
import Paths_astview (getDataFileName)

-- |builds initial gui state from glade xml file
gladeToGUI :: GladeXML -> IO GUI
gladeToGUI xml = do
  win   <- xmlGetWidget xml castToWindow "mainWindow"
  treeview <- xmlGetWidget xml castToTreeView "treeview"
  tb <- buildSourceView =<< xmlGetWidget xml castToScrolledWindow "swSource"
  dialogAbout <-xmlGetWidget xml castToAboutDialog "dlgAbout"
  return $ GUI win treeview tb dialogAbout

-- |creates initial program state and provides an IORef to that
buildState :: GladeXML -> IO (IORef AstState)
buildState xml = do
  g <- gladeToGUI xml
  let astSt = AstState st g defaultValue
      st = defaultValue { knownLanguages = languages}
  newIORef astSt

-- | initiates gui and returns initial program state
setupGUI :: IO (IORef AstState)
setupGUI = do
  initGUI
  Just xml <- xmlNew =<< getDataFileName ("data" </> "astview.glade")
  r <- buildState xml

  hookNonGuiStateWidgets xml r
  hooks r
  mapM_ (registerMenuAction xml r) menuActions
  return r

-- -------------------------------------------------------------------
-- ** some helper functions
-- -------------------------------------------------------------------

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

-- | registers one GUIAction with a MenuItem
registerMenuAction
  :: GladeXML -> IORef AstState
  -> (String,AstAction ()) -> IO (ConnectId MenuItem)
registerMenuAction xml ref (gtkId,action) = do
  item <- xmlGetWidget xml castToMenuItem gtkId
  onActivateLeaf item $ action ref

-- *** hooks for widgets which are not part of the 'Gui' type

{- |We distinguish widgets by the property whether they are part of the type 'Gui'. 
Widgets, which occur in type 'Gui', can be accessed by functions binded to the 
menu items (see module 'Language.Astview.Gui.Actions' for details). Other widgets
(like the check menu item for flattening lists for example) only need to be 
connected to their respective actions, but do not need to be 
directly accessed by other widgets actions.

This distinction keeps the type 'Gui' and thus the whole program state 
'State' as small as possible.
-}
hookNonGuiStateWidgets :: GladeXML -> AstAction ()
hookNonGuiStateWidgets xml ref = do
  initFlattenCheckMenuItem xml ref
  return ()

-- |bind the check menu for flattening lists to the boolean value in the state.
initFlattenCheckMenuItem :: GladeXML -> AstAction (ConnectId CheckMenuItem)
initFlattenCheckMenuItem xml ref = do
  
  isFlat <- getFlattenLists ref
  mFlatten <- xmlGetWidget xml castToCheckMenuItem "mFlatten" 
  checkMenuItemSetActive mFlatten isFlat

  mFlatten `on` checkMenuItemToggled $ do
    isActive <- checkMenuItemGetActive mFlatten
    setFlattenLists isActive ref

-- *** hooks for widgets which are part the 'Gui' type

-- | adds actions to widgets defined in type 'Gui'. (see 'hookNonGuiStateWidgets')
hooks :: AstAction (ConnectId Window)
hooks ref = do
  textbuffer <- getSourceBuffer ref
  storeLastActiveTextPosition textbuffer ref

  tree <- getTreeView ref
  storeLastActiveTreePosition tree ref

  win <- getWindow ref
  controlPtoReparse win ref
  closeAstviewOnWindowClosed win ref
  close win ref

type Hook a = a -> AstAction (ConnectId a)

-- |stores the last active cursor position in text to the program state
storeLastActiveTextPosition :: Hook SourceBuffer
storeLastActiveTextPosition buffer ref = onBufferChanged buffer $ do
    actionBufferChanged ref
    cp <- getCursorPosition ref
    setCursor cp ref

-- |stores the path to the last selected tree cell to the program state
storeLastActiveTreePosition :: Hook TreeView
storeLastActiveTreePosition tree ref =
  onCursorChanged tree  $ do
    (p,_) <- treeViewGetCursor tree
    setTreePath p ref

-- |bind ctrl+p to the reparse action
controlPtoReparse :: Hook Window
controlPtoReparse w ref =
  w `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "p" <- eventKeyName
    liftIO $ actionReparse ref

-- |softly terminate application on main window closed
closeAstviewOnWindowClosed :: Hook Window
closeAstviewOnWindowClosed w ref =
  w `on` deleteEvent $ tryEvent $ liftIO $ actionQuit ref

-- |terminate application on main window closed
close :: Hook Window
close w _ = onDestroy w mainQuit
