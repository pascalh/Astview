{- provides 'setupGUI' the main gui initialization
function
 -
 -}
module Language.Astview.Gui.Init(setupGUI) where

import Language.Astview.Gui.Types
import Language.Astview.Gui.Actions
import Language.Astview.Languages(languages)
import Language.Astview.Language

import Data.List(intercalate)
import Control.Monad(void,forM_,when,guard)
import Control.Monad.Trans (liftIO)
import Data.IORef
import System.FilePath ((</>))

import Graphics.UI.Gtk hiding (Language)
import Graphics.UI.Gtk.SourceView
import Paths_astview (getDataFileName)


builderGetObjectStr :: GObjectClass cls	=> Builder	-> (GObject -> cls)	-> String	 -> IO cls
builderGetObjectStr = builderGetObject

-- |builds initial gui state from glade builder file
gladeToGUI :: Builder -> IO GUI
gladeToGUI builder = do
  win   <- builderGetObjectStr builder castToWindow "mainWindow"
  treeview <- builderGetObjectStr builder castToTreeView "treeview"
  tb <- buildSourceView =<< builderGetObjectStr builder castToScrolledWindow "swSource"
  return $ GUI win treeview tb

-- |creates initial program state and provides an IORef to that
buildState :: Builder -> IO (IORef AstState)
buildState builder = do
  g <- gladeToGUI builder
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

  hookNonGuiStateWidgets builder r
  hooks r
  mapM_ (registerMenuAction builder r) menuActions
  return r

-- |the association between the gui functions from 'Actions'
-- and the gtk identifier from glade file.
menuActions :: [(String,AstAction ())]
menuActions =
  [("mNew",actionEmptyGUI)
  ,("mReparse",actionReparse)
  ,("mSaveAs",actionSaveAs)
  ,("mOpen",actionDlgOpen)
  ,("mSave",actionSave)
  ,("mCut",actionCutSource)
  ,("mCopy",actionCopySource)
  ,("mPaste",actionPasteSource)
  ,("mDelete",actionDeleteSource)
  ,("mSrcLoc",actionJumpToSrcLoc)
  ,("mTextLoc",actionJumpToTextLoc)
  ,("mQuit",actionQuit)
  ]

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
  :: Builder -> IORef AstState
  -> (String,AstAction ()) -> IO (ConnectId MenuItem)
registerMenuAction builder ref (gtkId,action) = do
  item <- builderGetObjectStr builder castToMenuItem gtkId
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
hookNonGuiStateWidgets :: Builder -> AstAction ()
hookNonGuiStateWidgets builder ref = void $ do
  initLanguagesMenu builder ref
  initFlattenCheckMenuItem builder ref
  initAboutDialog builder ref

-- |sets up the menu @Languages@ and binds actions to the menu items.
initLanguagesMenu :: Builder -> AstAction ()
initLanguagesMenu builder ref = do
  mAuto <- builderGetObjectStr builder castToRadioMenuItem "mLangAuto"
  mAuto `on` checkMenuItemToggled $ do
    isActive <- checkMenuItemGetActive mAuto
    when isActive $ do
      setActiveLanguage Nothing ref
      actionReparse ref

  languages <- getKnownLanguages ref
  guard $ not $ null languages
  menu <- builderGetObjectStr builder castToMenu "menuLanguages"
  forM_ (zip languages [0..]) $ \(language,position) -> do
    item <- radioMenuItemNewWithLabelFromWidget mAuto (makeLanguageLabel language)
    menuAttach menu item 0 1 (2+position) (3+position)
    item `on` checkMenuItemToggled $ do
      setActiveLanguage (Just language) ref
      actionReparse ref

-- |produces a string containing the languages' name and
-- the associated file extensions
makeLanguageLabel :: Language -> String
makeLanguageLabel language =
  name language ++
  "   [" ++
  intercalate "," (map (\l -> "*"++l) $ exts language)++
  "]"

-- |bind the check menu for flattening lists to the boolean value in the state.
initFlattenCheckMenuItem :: Builder -> AstAction (ConnectId CheckMenuItem)
initFlattenCheckMenuItem builder ref = do

  isFlat <- getFlattenLists ref
  mFlatten <- builderGetObjectStr builder castToCheckMenuItem "mFlatten"
  checkMenuItemSetActive mFlatten isFlat

  mFlatten `on` checkMenuItemToggled $ do
    isActive <- checkMenuItemGetActive mFlatten
    setFlattenLists isActive ref

-- |setup about dialog
initAboutDialog :: Builder -> AstAction (ConnectId MenuItem)
initAboutDialog builder ref =
  registerMenuAction builder ref ("mAbout",action) where
    action _ = do
      dialog <- builderGetObjectStr builder castToAboutDialog "dlgAbout"
      aboutDialogSetUrlHook (\(_ :: String) -> return ())
      widgetShow dialog
      dialog `onResponse` const (widgetHide dialog)
      return ()

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
