{- contains the main GUI functions
 -
 -}
module Language.Astview.GUI where

-- guiactions
import Language.Astview.GUIData
import Language.Astview.GUIActions 

-- base
import Control.Monad.Trans (liftIO)

-- state
import Data.IORef (IORef,newIORef,writeIORef,readIORef)

-- filepath
import System.FilePath ((</>))

-- gtk
import Graphics.UI.Gtk  
import Graphics.UI.Gtk.Gdk.EventM

-- glib
import System.Glib.Signals (ConnectId)

-- glade
import Graphics.UI.Gtk.Glade     

-- gtksourceview
import Graphics.UI.Gtk.SourceView

-- astview-utils
import Language.Astview.Parser

-- generated on-the-fly by cabal
import Paths_astview (getDataFileName) 

-- | builds the GUI 
buildGUI :: [Parser] -> IO GUI
buildGUI parsers = do
  -- GTK init
  initGUI 

  -- load GladeXML
  Just xml <- xmlNew =<< getDataFileName ("data" </> "astview.glade")
 
  -- get or create widgets
  window   <- xmlGetWidget xml castToWindow "mainWindow"
  treeview <- xmlGetWidget xml castToTreeView "treeview"

  tb <- buildSourceView 
    =<< xmlGetWidget xml castToScrolledWindow "swSource" 

  dlgAbout <-xmlGetWidget xml castToAboutDialog "dlgAbout"

  -- storage for current file
  rFile <- newIORef unsavedDoc
  -- 
  rChanged <- newIORef False
  -- storage for all parsers
  rParsers <- newIORef parsers
  -- storage for current parser 
  rCurParser <- newIORef $ head parsers

  -- setup combobox
  vbox <- xmlGetWidget xml castToVBox "vboxMain"
  cbox <- comboBoxNewText
  containerAdd vbox cbox
  boxSetChildPacking vbox cbox PackNatural 2 PackEnd
  mapM_ (comboBoxAppendText cbox . buildLabel) parsers 

  -- build compound datatype
  let gui = GUI {
      window=window 
    , tv=treeview 
    , tb=tb
    , rFile=rFile
    , rChanged=rChanged
    , rParsers=rParsers
    , rCurParser=rCurParser
    , dlgAbout=dlgAbout
    , cbox=cbox
    }

  -- get all menuitems from xml and register guiactions to them
  mapM_ (registerMenuAction xml gui) menuActions
  
  -- add hooks to buttons
  hooks gui 

  -- finally return gui
  return gui

-- -------------------------------------------------------------------
-- ** some helper functions
-- -------------------------------------------------------------------

-- |builds combobox label for a parser
buildLabel :: Parser -> String
buildLabel parser = 
  name parser
  ++ " ["
  ++ concatMap (" "++) (exts parser)
  ++ "]"

-- | setup the GtkSourceView and add it to the ScrollPane. return the 
-- underlying textbuffer
buildSourceView :: ScrolledWindow -> IO SourceBuffer
buildSourceView sw = do
  sourceBuffer <- sourceBufferNew Nothing
  sourceBufferSetHighlightSyntax sourceBuffer True
  sourceView <- sourceViewNewWithBuffer sourceBuffer
  sourceViewSetShowLineNumbers sourceView True
  sourceViewSetHighlightCurrentLine sourceView True
  srcfont <- fontDescriptionFromString "Monospace 10"
  widgetModifyFont sourceView (Just srcfont)
  containerAdd sw sourceView
  return sourceBuffer

-- | registers one GUIAction with a MenuItem
registerMenuAction 
  :: GladeXML -> GUI -> MenuAction -> IO (ConnectId MenuItem)
registerMenuAction xml gui (gtkId,guiaction) = do
  item <- xmlGetWidget xml castToMenuItem gtkId
  onActivateLeaf item (guiaction gui)

-- | adds actions to some widgets
hooks :: GUI -> IO (ConnectId Window)
hooks gui = do
  -- textbuffer
  onBufferChanged (tb gui) (actionBufferChanged gui)
  
  -- ctrl+p to reparse
  (window gui) `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "p" <- eventKeyName
    liftIO $ actionReparse gui 

  (cbox gui) `on` changed $ do
    i <- comboBoxGetActive (cbox gui) 
    parsers <- readIORef (rParsers gui)
    let parser = parsers!!i
    writeIORef (rCurParser gui) parser 
    comboBoxSetActive (cbox gui) i
    actionParse parser gui

  (dlgAbout gui) `onResponse` (\ _ -> widgetHide (dlgAbout gui) )
        
  (window gui) `on` deleteEvent $ tryEvent $ liftIO $ actionQuit gui
  
  -- window    
  onDestroy (window gui) mainQuit
    

