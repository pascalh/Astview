module Language.Astview.GUI where

-- base
import Prelude hiding (writeFile)
import Data.Maybe(fromJust,isJust)
import Data.List (find,findIndex)
import Control.Monad ((=<<),when,liftM,filterM)
import Data.Char (toLower)
import Control.Monad.Trans (liftIO)

-- io
import System.IO 
  (IOMode,withFile,hGetContents,IOMode(..),hPutStr,hClose)
import System.IO.Error

-- state
import Data.IORef

-- filepath
import System.FilePath 
  (takeExtension,splitFileName,pathSeparator,joinPath)
import System.Directory (doesDirectoryExist)

-- bytestring
import qualified Data.ByteString.Char8 as BS (hGetContents,unpack)

-- containers
import Data.Tree ( Tree(Node,rootLabel) )

-- gtk
import Graphics.UI.Gtk  -- (import all)
import Graphics.UI.Gtk.Gdk.EventM

-- glib
import System.Glib.Signals (ConnectId)

-- glade
import Graphics.UI.Gtk.Glade      -- (import all)
import Graphics.UI.Gtk.ModelView  -- (import all)

-- gtksourceview
import Graphics.UI.Gtk.SourceView -- (import all)

-- commands
import System.Cmd (rawSystem)

-- astview-utils
import Language.Astview.Parser

-- generated on-the-fly by cabal
import Paths_astview (getDataFileName,getDataDir) 


-- -------------------------------------------------------------------
-- * Main GUI types and functions
-- -------------------------------------------------------------------

-- | a simple container type to ease access to GUI components
data GUI = GUI {
    window :: Window              -- ^ main window
  , tv :: TreeView                -- ^ treeview
  , tb :: SourceBuffer            -- ^ sourceview
  , rFile :: IORef String         -- ^ current file
  , rChanged :: IORef Bool        -- ^ true if file changed
  , rParsers:: IORef [Parser]     -- ^ parsers
  , rCurParser :: IORef Parser    -- ^ current parser
  , dlgOpen :: FileChooserDialog  -- ^ filechooser (fc)
  , dlgSave :: FileChooserDialog  -- ^ filechooser (save as)
  , dlgAbout :: AboutDialog       -- ^ about dialog
  , btnOpenOpen :: Button         -- ^ open button of the fc
  , btnOpenCancel :: Button       -- ^ cancel button of the fc
  , btnSaveSave :: Button         -- ^ save as button of dlg_SaveAs
  , btnSaveCancel :: Button       -- ^ cancel button of dlg_SaveAs
  , entryName :: Entry            -- ^ text entry of dlg_SaveAs
  , cbox :: ComboBox
  }

-- | takes a reference to the compound GUI type and performs
--   some action on the GUI. see section GUI-Actions below
type GUIAction = GUI -> IO ()

-- | pairs a gtk-id with a GUIAction to easily map over all MenuItems
type MenuAction = (String,GUIAction)

-- | a list of pairs of gtk-ids and GUIActions 
menuActions :: [MenuAction]
menuActions = 
  [("mNew",actionEmptyGUI)
  ,("mOpen",actionDlgOpenRun)
  ,("mParse",actionReparse)
  ,("mSave",actionSave)
  ,("mSaveAs",actionDlgSaveRun)
  ,("mCut",actionCutSource)
  ,("mCopy",actionCopySource)
  ,("mPaste",actionPasteSource)
  ,("mDelete",actionDeleteSource)
  ,("mAbout",actionAbout)
  ,("mShowHelp",actionHelp)
  ,("mQuit",actionQuit)
  ]

-- | builds the GUI 
buildGUI :: [Parser] -> IO GUI
buildGUI parsers = do
  -- GTK init
  initGUI 

  -- load GladeXML
  Just xml <- xmlNew =<< getDataFileName "data/astview.glade"
 
  -- get or create widgets
  window   <- xmlGetWidget xml castToWindow "mainWindow"
  treeview <- xmlGetWidget xml castToTreeView "treeview"

  tb <- buildSourceView 
    =<< xmlGetWidget xml castToScrolledWindow "swSource" 

  dlgOpen <-xmlGetWidget xml castToFileChooserDialog "dlgOpen"
  dlgSave <-xmlGetWidget xml castToFileChooserDialog "dlgSave"
  dlgAbout <-xmlGetWidget xml castToAboutDialog "dlgAbout"
  btnOpenOpen <- xmlGetWidget xml castToButton "btnOpenOpen"
  btnOpenCancel <- xmlGetWidget xml castToButton "btnOpenCancel"
  btnSaveSave <- xmlGetWidget xml castToButton "btnSaveSave"
  btnSaveCancel <- xmlGetWidget xml castToButton "btnSaveCancel"
  entryName <- xmlGetWidget xml castToEntry "entryName"

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
    , dlgOpen=dlgOpen
    , dlgSave=dlgSave
    , dlgAbout=dlgAbout
    , btnOpenOpen=btnOpenOpen 
    , btnOpenCancel=btnOpenCancel 
    , btnSaveSave=btnSaveSave
    , btnSaveCancel=btnSaveCancel
    , entryName=entryName
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

-- |suffix of window title
suffix :: String
suffix = " - astview"

-- |unsaved document
unsavedDoc :: String
unsavedDoc = "Unsaved document"

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
  -- filechooser open
  onClicked (btnOpenOpen gui) (actionLoadChooser gui)
  onClicked (btnOpenCancel gui) (widgetHide (dlgOpen gui) )
  afterFileActivated (dlgOpen gui) (actionLoadChooser gui)
  -- filechooser save
  onClicked (btnSaveSave gui) (actionSaveAs gui)
  onClicked (btnSaveCancel gui) (widgetHide (dlgSave gui))
  onUpdatePreview (dlgSave gui) (actionUpdateName gui) 
  
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
    actionParse (parser) gui

  (dlgAbout gui) `onResponse` (\ _ -> widgetHide (dlgAbout gui) )
        
  (window gui) `on` deleteEvent $ tryEvent $ do
    liftIO $ actionQuit gui
  
  -- window    
  onDestroy (window gui) mainQuit
    
-- -------------------------------------------------------------------
-- * GUI Actions
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- ** filemenu
-- -------------------------------------------------------------------

-- | resets the GUI, 
actionEmptyGUI :: GUIAction
actionEmptyGUI gui = do
  maybeCol <- treeViewGetColumn (tv gui) 0
  case maybeCol of
    Just col-> treeViewRemoveColumn (tv gui) col
    Nothing -> return undefined
  textBufferSetText (tb gui) ""
  writeIORef (rFile gui) unsavedDoc
  writeIORef (rChanged gui) False
  windowSetTitle (window gui) (unsavedDoc++suffix)

-- | updates the sourceview with a given file, chooses a parser by 
-- extension and parses the file
actionLoadHeadless :: FilePath -> GUIAction
actionLoadHeadless file gui = 
  catch 
    (do
      parsers <- readIORef (rParsers gui) 
      writeIORef (rFile gui) file
      contents <- withFile 
        file ReadMode (fmap BS.unpack . BS.hGetContents)
      textBufferSetText (tb gui) contents

      let filename = snd $ splitFileName file
      windowSetTitle (window gui) (filename ++ suffix)
      let e = takeExtension file
      whenJust 
        (find (elem e . exts) parsers) $
        \parser -> do   
          activateParser parser gui
          actionParse parser gui
          writeIORef (rChanged gui) False 
    )
    print

-- | helper for loadHeadless
activateParser :: Parser -> GUIAction
activateParser parser gui = do
  writeIORef (rCurParser gui) parser
  parsers <- readIORef (rParsers gui) 
  case findIndex (parser==) parsers of
    Just i -> comboBoxSetActive (cbox gui) i
    Nothing-> return ()

-- | helper for loadHeadless
setupSyntaxHighlighting :: Parser -> GUIAction
setupSyntaxHighlighting parser gui = do
  langManager <- sourceLanguageManagerGetDefault
  maybeLang <- sourceLanguageManagerGetLanguage 
    langManager 
    (map toLower (name parser))
  case maybeLang of
    Just l -> do
      sourceBufferSetHighlightSyntax (tb gui) True
      sourceBufferSetLanguage (tb gui) l 
    Nothing-> 
      sourceBufferSetHighlightSyntax (tb gui) False   

  
-- | gets filename from filechooser and delegates to 
-- actionLoadHeadless
actionLoadChooser :: GUIAction
actionLoadChooser gui = do
  whenJustM
    (fileChooserGetFilename (dlgOpen gui)) $ 
    \file -> do
      actionLoadHeadless file gui
      widgetHide (dlgOpen gui)

-- | parses the contents of the sourceview with the selected parser
actionParse :: Parser -> GUIAction
actionParse parser gui = do
  writeIORef (rCurParser gui) parser
  sourceBufferSetHighlightSyntax (tb gui) True
  setupSyntaxHighlighting parser gui
  plain <- getText gui
  maybeCol <- treeViewGetColumn (tv gui) 0
  case maybeCol of
    Just col-> treeViewRemoveColumn (tv gui) col
    Nothing -> return (-1)
  let tree' = (tree parser) plain
  model <- treeStoreNew [Node "" [tree']]
  treeViewSetModel (tv gui) model
  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True 
  cellLayoutSetAttributes 
    col 
    renderer 
    model 
    (\row -> [ cellText := row ] )
  treeViewAppendColumn (tv gui) col 
  return ()

-- |saves file to url saved in clipboard
actionSave :: GUIAction
actionSave gui = do
  file <- readIORef (rFile gui)
  text <- getText gui
  actionSaveWorker gui text file

-- |saves curren file if a file is active or calls "save as"-dialog
actionSaveWorker :: GUI -> String -> String -> IO ()
actionSaveWorker gui plain file =
  case file of
    "Unsaved document"  -> actionDlgSaveRun gui
    otherwise           -> do 
      deleteStar gui
      writeIORef (rChanged gui) False
      writeFile file plain 
 
-- |saves file with filename given by textentry
actionSaveAs :: GUIAction
actionSaveAs gui =
  whenJustM
    (fileChooserGetCurrentFolder (dlgSave gui)) $
    \path -> do
      name <- entryGetText (entryName gui)
      let filepath = (path++[pathSeparator]++name)
      writeIORef (rFile gui) filepath
      writeIORef (rChanged gui) False
      writeFile filepath =<< getText gui
      windowSetTitle (window gui) (name++suffix)
      widgetHide (dlgSave gui)
  
-- |updates the textview after selecting a file in save as filechooser
actionUpdateName :: GUIAction
actionUpdateName gui = do
  whenJustM (fileChooserGetFilename (dlgSave gui)) $ \ path -> do
    isDir <- doesDirectoryExist path
    when (not isDir) $
      let (_,file) = splitFileName path in 
      entrySetText (entryName gui) file

-- -------------------------------------------------------------------
-- ** edit menu
-- -------------------------------------------------------------------

-- |moves selected source to clipboard (cut)
actionCutSource :: GUIAction  
actionCutSource gui = do
  actionCopySource gui
  actionDeleteSource gui
 
-- |copies selected source to clipboard  
actionCopySource :: GUIAction
actionCopySource gui = do
  (start,end) <- textBufferGetSelectionBounds (tb gui) 
  clipBoard <- clipboardGet selectionClipboard
  clipboardSetText 
    clipBoard 
    =<< textBufferGetText (tb gui) start end True

-- |pastes text from clipboard at current cursor position  
actionPasteSource :: GUIAction
actionPasteSource gui = do
  clipBoard <- clipboardGet selectionClipboard
  clipboardRequestText clipBoard (insertAt (tb gui)) where
    insertAt :: SourceBuffer -> Maybe String -> IO ()
    insertAt tb m = whenJust m (textBufferInsertAtCursor tb)

-- |deletes selected source
actionDeleteSource :: GUIAction
actionDeleteSource gui = 
  textBufferDeleteSelection (tb gui) False False >> return ()



-- -------------------------------------------------------------------
-- ** help menu
-- -------------------------------------------------------------------

-- | shows the help dialog
actionHelp :: GUIAction
actionHelp gui = do
  helpfile <- getDataFileName "data/astview.html"
  dir <- getDataDir
  rawSystem "firefox" [joinPath [dir,helpfile]]
  return ()
    
-- | launches info dialog
actionAbout :: GUIAction
actionAbout gui = do
  aboutDialogSetUrlHook (\_ -> return ())
  licensefile <- getDataFileName "data/LICENSE.unwrapped"
  contents <- catch 
    (withFile licensefile ReadMode ((fmap BS.unpack) . BS.hGetContents))
    (\ioe -> return $ "Err" ++ (show ioe))
  aboutDialogSetWrapLicense (dlgAbout gui) True 
  aboutDialogSetLicense (dlgAbout gui) (Just contents)
  widgetShow (dlgAbout gui)


-- -------------------------------------------------------------------
-- ** other actions and helpers
-- -------------------------------------------------------------------

-- |similar to *when*. 
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m action =
  when (isJust m) ((action.fromJust) m) 
  
-- |similar to *whenJust*, but value is inside a monad
whenJustM :: Monad m => m(Maybe a) -> (a -> m ()) -> m ()
whenJustM val action = do
  m <- val
  when (isJust m) ((action.fromJust) m)   

-- | adds '*' to window title if file changed and sets state
actionBufferChanged :: GUIAction
actionBufferChanged gui = do
  writeIORef (rChanged gui) True
  t <- windowGetTitle (window gui)
  when (head t /= '*') (windowSetTitle (window gui) ('*':t))


-- | destroys window widget 
actionQuit :: GUIAction
actionQuit gui = do 
  changed <- readIORef (rChanged gui) 
  if changed 
    then do 
      dia <- dialogNew
      dialogAddButton dia stockYes ResponseYes
      dialogAddButton dia stockNo ResponseNo
      dialogAddButton dia stockCancel ResponseCancel
      contain <- dialogGetUpper dia
  
      windowSetTitle dia "astview"
      containerSetBorderWidth dia 2
      file <- readIORef (rFile gui)
      lbl <- labelNew 
        (Just $ "Save changes to document \""++
                (snd $ splitFileName file) ++
                "\" before closing?")
      boxPackStartDefaults contain lbl

      widgetShowAll dia
      response <- dialogRun dia
      case response of 
        ResponseYes    -> actionSave gui
        ResponseCancel -> return ()
        ResponseNo     -> widgetDestroy (window gui)
      widgetHide dia
    else widgetDestroy (window gui)
  

-- | launches open dialog
actionDlgOpenRun :: GUIAction
actionDlgOpenRun gui = dialogRun (dlgOpen gui) >> return ()

-- | launches save dialog
actionDlgSaveRun :: GUIAction
actionDlgSaveRun gui = do
  setupDlgSave gui =<< readIORef (rFile gui)
  dialogRun (dlgSave gui) >> return ()

-- |set current directory and filename in dlgSave (setup)
setupDlgSave :: GUI -> String -> IO ()
setupDlgSave gui s = do 
  let (dir,file) = splitFileName s
  fileChooserSetFilename (dlgSave gui) dir
  entrySetText (entryName gui) file

-- |applies current parser to current sourcebuffer 
actionReparse :: GUIAction
actionReparse gui = do
  parser <- readIORef (rCurParser gui)
  activateParser parser gui
  actionParse parser gui

-- |removes @*@ from window title if existing  
deleteStar :: GUIAction
deleteStar gui = do
  t <- windowGetTitle (window gui)
  when (head t == '*') (windowSetTitle (window gui) (tail t))

-- | helper for various text-processing actions
getText :: GUI -> IO String
getText gui = do
  start <- textBufferGetStartIter (tb gui)
  end <- textBufferGetEndIter (tb gui)
  textBufferGetText (tb gui) start end True
  
-- |safe function to write files
writeFile :: FilePath -> String -> IO ()
writeFile f str = catch
  (withFile f WriteMode (\h -> hPutStr h str >> hClose h))
  (putStrLn . show)

