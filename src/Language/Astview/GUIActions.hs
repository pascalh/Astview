{- contains the GUIActions connected to menuItems
 -
 -}

module Language.Astview.GUIActions where

-- gui data types
import Language.Astview.GUIData 

-- base
import Prelude hiding (writeFile,Right)
import Data.Maybe(fromJust,isJust)
import Data.List (find,findIndex)
import Control.Monad ((=<<),when)
import Data.Char (toLower)

-- io
import System.IO (withFile,IOMode(..),hPutStr,hClose)

-- state
import Data.IORef (IORef,writeIORef,readIORef)

-- filepath
import System.FilePath ((</>),takeExtension,takeFileName)
import System.Directory (doesDirectoryExist)

-- bytestring
import qualified Data.ByteString.Char8 as BS (hGetContents,unpack)

-- containers
import Data.Tree ( Tree(Node,rootLabel) )

-- gtk
import Graphics.UI.Gtk  

-- gtksourceview
import Graphics.UI.Gtk.SourceView 

-- commands
import System.Cmd (rawSystem)

-- astview-utils
import Language.Astview.Parser

-- generated on-the-fly by cabal
import Paths_astview (getDataFileName,getDataDir) 

-- |suffix of window title
suffix :: String
suffix = " - astview"

-- |unsaved document
unsavedDoc :: String
unsavedDoc = "Unsaved document"

-- | a list of pairs of gtk-ids and GUIActions 
menuActions :: [MenuAction]
menuActions = 
  [("mNew",actionEmptyGUI)
  ,("mOpen",actionDlgOpenRun)
  ,("mParse",actionReparse)
  ,("mPath",actionGetPath)
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


-- -------------------------------------------------------------------
-- * filemenu menu actions
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

      windowSetTitle (window gui) ((takeFileName file) ++ suffix)
      whenJust 
        (find (elem (takeExtension file) . exts) parsers) $
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
  model <- treeStoreNew [Node "" [(tree parser) plain]]
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
  where 
    setupSyntaxHighlighting :: Parser -> GUIAction
    setupSyntaxHighlighting parser gui = do
      langManager <- sourceLanguageManagerGetDefault
      maybeLang <- sourceLanguageManagerGetLanguage 
        langManager 
        (map toLower $ syntax parser)
      case maybeLang of
        Just l -> do
          sourceBufferSetHighlightSyntax (tb gui) True
          sourceBufferSetLanguage (tb gui) l 
        Nothing-> 
          sourceBufferSetHighlightSyntax (tb gui) False   

-- |saves file 
actionSave :: GUIAction
actionSave gui = do
  text <- getText gui
  actionSaveWorker gui text =<< readIORef (rFile gui)
 
-- |saves current file if a file is active or calls "save as"-dialog
actionSaveWorker :: GUI -> String -> String -> IO ()
actionSaveWorker gui plain file =
  case file of
    "Unsaved document"  -> actionDlgSaveRun gui
    otherwise           -> do 
      deleteStar gui
      writeIORef (rChanged gui) False
      writeFile file plain 
      where
        -- |removes @*@ from window title if existing  
        deleteStar :: GUIAction
        deleteStar gui = do
          t <- windowGetTitle (window gui)
          when (head t == '*') 
               (windowSetTitle (window gui) (tail t))
 
-- -------------------------------------------------------------------
-- ** editmenu menu actions
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
-- ** helpmenu menu actions
-- -------------------------------------------------------------------

-- |opens help in firefox
actionHelp :: GUIAction
actionHelp gui = do
  helpfile <- getDataFileName ("data" </> "astview.html")
  dir <- getDataDir
  rawSystem "firefox" [dir </> helpfile]
  return ()
    
-- | launches info dialog
actionAbout :: GUIAction
actionAbout gui = do
  aboutDialogSetUrlHook (\_ -> return ())
  licensefile <- getDataFileName ("data" </> "LICENSE.unwrapped")
  contents <- catch 
    (withFile licensefile ReadMode ((fmap BS.unpack) . BS.hGetContents))
    (\ioe -> return $ "Err" ++ (show ioe))
  aboutDialogSetWrapLicense (dlgAbout gui) True 
  aboutDialogSetLicense (dlgAbout gui) (Just contents)
  widgetShow (dlgAbout gui)

-- -------------------------------------------------------------------
-- ** other actions 
-- -------------------------------------------------------------------

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
                (takeFileName file) ++
                "\" before closing?")
      boxPackStartDefaults contain lbl

      widgetShowAll dia
      response <- dialogRun dia
      case response of 
        ResponseYes    -> actionSave gui>>widgetDestroy (window gui)
        ResponseCancel -> return ()
        ResponseNo     -> widgetDestroy (window gui)
      widgetHide dia
    else widgetDestroy (window gui)
  

-- | launches open dialog
actionDlgOpenRun :: GUIAction
actionDlgOpenRun gui = do 
  dia <- fileChooserDialogNew 
    (Just "astview") 
    Nothing 
    FileChooserActionOpen 
    []
  dialogAddButton dia stockCancel ResponseCancel
  dialogAddButton dia stockOpen ResponseOk

  widgetShowAll dia
  response <- dialogRun dia
  case response of 
    ResponseCancel -> return ()
    ResponseOk     -> 
      whenJustM
        (fileChooserGetFilename dia) $ 
        \file -> actionLoadHeadless file gui
  widgetHide dia

-- | launches save dialog
actionDlgSaveRun :: GUIAction
actionDlgSaveRun gui = do
  file <- readIORef (rFile gui)
  dia <- fileChooserDialogNew 
    (Just "astview") 
    Nothing 
    FileChooserActionSave 
    []
  dialogAddButton dia stockCancel ResponseCancel
  dialogAddButton dia stockOpen ResponseOk

  widgetShowAll dia
  response <- dialogRun dia
  case response of 
    ResponseCancel -> return ()
    ResponseOk     -> 
      whenJustM
        (fileChooserGetFilename dia) $ 
        \file -> do 
          writeIORef (rFile gui) file
          writeIORef (rChanged gui) False
          writeFile file =<< getText gui
          windowSetTitle 
            (window gui) 
            ((takeFileName file)++suffix)
  widgetHide dia

-- |applies current parser to current sourcebuffer 
actionReparse :: GUIAction
actionReparse gui = do
  parser <- readIORef (rCurParser gui)
  activateParser parser gui
  actionParse parser gui

data Direction 
  = Down -- ^ go down one level to the leftmost child
  | Right -- ^ stay at the same level and go to the right
  deriving Show

actionGetPath :: GUIAction  
actionGetPath gui = do
  s <- treeViewGetSelection $ tv gui
  t <- fmap (tail.head) $ treeSelectionGetSelectedRows s
  putStr $ show t ++ " : " 
  putStrLn $ show $ trans t where
    -- transforms gtk2hs path representation to direction
    trans :: [Int] -> [Direction]
    trans (x:xs) = Down : replicate x Right ++ trans xs
    trans [] = []

-- -------------------------------------------------------------------
-- ** Helpers
-- -------------------------------------------------------------------

-- |similar to @when@ 
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m action =
  when (isJust m) ((action.fromJust) m) 
  
-- |similar to @whenJust@, but value is inside a monad
whenJustM :: Monad m => m(Maybe a) -> (a -> m ()) -> m ()
whenJustM val action = do
  m <- val
  when (isJust m) ((action.fromJust) m)  

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

