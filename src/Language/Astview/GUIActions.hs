{- contains the GUIActions connected to menuItems
 -
 -}

module Language.Astview.GUIActions where

-- gui data types
import Language.Astview.GUIData 

-- base
import Prelude hiding (writeFile)
import Data.Maybe(fromJust,isJust)
import Data.List (find)
import Control.Monad (when)
import Data.Char (toLower)
-- io
import System.IO (withFile,IOMode(..),hPutStr,hClose)

-- filepath
import System.FilePath ((</>),takeExtension,takeFileName)

-- bytestring
import qualified Data.ByteString.Char8 as BS (hGetContents,unpack)

-- containers
import Data.Tree ( Tree(Node) )

-- gtk
import Graphics.UI.Gtk hiding (Language,get,response,bufferChanged) 

-- gtksourceview
import Graphics.UI.Gtk.SourceView 

-- commands
import System.Cmd (rawSystem)

-- astview-utils
import Language.Astview.Language 
import Language.Astview.SourceLocation hiding (line,row)

-- generated on-the-fly by cabal
import Paths_astview (getDataFileName,getDataDir) 

-- |unsaved document
unsavedDoc :: String
unsavedDoc = "Unsaved document"

-- | a list of pairs of gtk-ids and GUIActions 
menuActions :: [(String,AstAction ())]
menuActions = 
  [("mNew",actionEmptyGUI)
  ,("mReparse",actionReparse)
  ,("mSaveAs",actionSaveAs)
  ,("mOpen",actionDlgOpenRun)
  ,("mSave",actionSave)
  ,("mCut",actionCutSource)
  ,("mCopy",actionCopySource)
  ,("mPaste",actionPasteSource)
  ,("mDelete",actionDeleteSource)
  ,("mSrcLoc",actionJumpToSrcLoc)
  ,("mPath",actionShowPath)
  ,("mAbout",actionAbout)
  ,("mShowHelp",actionHelp)
  ,("mQuit",actionQuit)
  ]


-- -------------------------------------------------------------------
-- * filemenu menu actions
-- -------------------------------------------------------------------

clearTreeView :: TreeView -> IO ()
clearTreeView t = do
  c <- treeViewGetColumn t 0
  case c of 
    Just col-> treeViewRemoveColumn t col
    Nothing -> return 0 
  return ()

-- | resets the GUI, 
actionEmptyGUI :: AstAction ()
actionEmptyGUI ref = do
  g <- getGui ref
  clearTreeView =<< getTreeView ref
  flip textBufferSetText [] =<< getSourceBuffer ref
  windowSetTitleSuffix (window g) unsavedDoc

-- | updates the sourceview with a given file, chooses a language by 
-- extension and parses the file
actionLoadHeadless :: FilePath -> AstAction ()
actionLoadHeadless file ref = do
  setcFile file ref
  s <- getAstState ref

  windowSetTitleSuffix 
    (window $ gui s) 
    (takeFileName file)
  contents <- withFile 
    file ReadMode (fmap BS.unpack . BS.hGetContents)
  buffer <- getSourceBuffer ref 
  textBufferSetText buffer contents
  deleteStar ref
  whenJustM
    (getLanguage ref) $
    \l -> actionParse l ref >> return ()

-- |tries to find a language based on the extension of 
-- current file name
getLanguage :: AstAction (Maybe Language)
getLanguage ref = do
  file <- getFile ref
  langs <- getLangs ref
  return $ find (elem (takeExtension file) . exts) langs

-- | parses the contents of the sourceview with the selected language
actionParse :: Language -> AstAction (Tree String)
actionParse l@(Language _ _ _ p to _ _) ref = do
  buffer <- getSourceBuffer ref
  view <- getTreeView ref
  sourceBufferSetHighlightSyntax buffer True
  setupSyntaxHighlighting buffer l
  plain <- getText buffer
  clearTreeView view

  -- error handling
  let t = case fmap to (p plain) of
           Left Err                  -> Node "Parse error" []
           Left (ErrMessage m)       -> Node m []
           Left (ErrLocation pos message) -> 
               Node ("Parse error at:"++show pos++": "++message) [] 
           Right ast                 -> ast
  
  model <- treeStoreNew [t]
  treeViewSetModel view model
  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True 
  cellLayoutSetAttributes 
    col 
    renderer 
    model 
    (\row -> [ cellText := row ] )
  treeViewAppendColumn view col 
  return t
    
-- |uses the name of given language to establish syntax highlighting in 
-- source buffer
setupSyntaxHighlighting :: SourceBuffer -> Language -> IO ()
setupSyntaxHighlighting buffer language = do
  langManager <- sourceLanguageManagerGetDefault
  maybeLang <- sourceLanguageManagerGetLanguage 
        langManager 
        (map toLower $ syntax language)
  case maybeLang of
    Just lang -> do
      sourceBufferSetHighlightSyntax buffer True
      sourceBufferSetLanguage buffer (Just lang) 
    Nothing -> sourceBufferSetHighlightSyntax buffer False   

-- |saves current file if a file is active or calls "save as"-dialog
actionSave :: AstAction ()
actionSave ref = do
  file <- getFile ref
  text <- getText =<< getSourceBuffer ref
  case file of
    "Unsaved document"  -> actionDlgSaveRun ref
    _                   -> do 
      deleteStar ref 
      writeFile file text 

-- |lanches the "save as"-dialog
actionSaveAs :: AstAction ()
actionSaveAs ref = do 
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
    ResponseOk     -> do
       maybeFile <- fileChooserGetFilename dia
       case maybeFile of
         Nothing-> return () 
         Just file -> writeFile file =<< getText =<< getSourceBuffer ref
    _ -> return ()
  widgetHide dia

-- |removes @*@ from window title if existing and updates state
deleteStar :: AstAction ()
deleteStar ref = do
  w <- getWindow ref
  t <- windowGetTitle w
  bufferChanged <- getChanged ref
  when bufferChanged (windowSetTitle w (tail t))
  setChanged False ref
 
-- -------------------------------------------------------------------
-- ** editmenu menu actions
-- -------------------------------------------------------------------

-- |moves selected source to clipboard (cut)
actionCutSource :: AstAction ()  
actionCutSource ref = do
  actionCopySource ref 
  actionDeleteSource ref 
  return ()

-- |copies selected source to clipboard  
actionCopySource :: AstAction () 
actionCopySource ref = do
  buffer <- getSourceBuffer ref
  (start,end) <- textBufferGetSelectionBounds buffer 
  clipBoard <- clipboardGet selectionClipboard
  clipboardSetText clipBoard =<< textBufferGetText buffer start end True

-- |pastes text from clipboard at current cursor position  
actionPasteSource :: AstAction ()
actionPasteSource ref = do 
  buffer <- getSourceBuffer ref
  clipBoard <- clipboardGet selectionClipboard
  clipboardRequestText clipBoard (insertAt buffer) where
    insertAt :: SourceBuffer -> Maybe String -> IO ()
    insertAt buff m = whenJust m (textBufferInsertAtCursor buff)

-- |deletes selected source
actionDeleteSource :: AstAction ()
actionDeleteSource ref = do
  buffer <- getSourceBuffer ref
  textBufferDeleteSelection buffer False False >> return ()

-- |returns the current cursor position in a source view.
-- return type: (line,row)
getCursorPosition :: AstAction CursorP
getCursorPosition ref = do
  (iter,_) <- textBufferGetSelectionBounds =<< getSourceBuffer ref
  l <- textIterGetLine iter
  r <- textIterGetLineOffset iter
  return $ CursorP (l+1) (r+1)

-- |opens tree position associated with current cursor position
actionJumpToSrcLoc :: AstAction ()
actionJumpToSrcLoc ref = do
  treePath <- actionGetSrcLoc ref 
  when (not $ null treePath) (selectPath treePath ref)

-- |returns the position in tree which is associated with the
-- current selected source location.
actionGetSrcLoc :: AstAction TreePath 
actionGetSrcLoc ref = do  
  (CursorP l r) <- getCursorPosition ref 
  
  -- reparse and set cursor in treeview
  maybeLang <- getLanguage ref
  case maybeLang of
    Nothing -> return [] 
    Just lang -> do 
      t <- actionParse lang ref 
      let sl = sourceLocations lang t
      let s = findSrcLoc l r $ map fst sl 
      return $ getAssociatedValue s sl 

-- |select tree path 
selectPath :: TreePath -> AstAction ()
selectPath p ref = do 
  view <- getTreeView ref
  treeViewExpandToPath view p
  treeViewSetCursor view p Nothing

-- |returns all source locations and paths to source
-- locations in current tree
sourceLocations :: Language -> Tree String -> [(SrcLocation,TreePath)]
sourceLocations lang = getSourceLocations lang . calcPaths [0] where
  calcPaths :: [Int] -> Tree String -> Tree (String,TreePath)
  calcPaths curPath (Node l cs) = 
    let paths = zipWith (\p e->p++[e]) (repeat curPath) [0,1..] in
    Node (l,curPath) (zipWith (\subtree p -> calcPaths p subtree) cs paths)

-- |a helper function for 'sourceLocations'
getSourceLocations :: Language 
                   -> Tree (String,TreePath) 
                   -> [(SrcLocation,TreePath)]
getSourceLocations l t@(Node (_,p) cs) =
  case srcLoc l of
    Just f -> 
      let xs = f $ fmap fst t in
      case xs of
        []    -> concatMap (getSourceLocations l) cs
        (x:_) -> [(x,p)]
    Nothing -> []


-- -------------------------------------------------------------------
-- ** helpmenu menu actions
-- -------------------------------------------------------------------

-- |opens help in firefox
actionHelp :: AstAction ()
actionHelp _ = do
  helpfile <- getDataFileName ("data" </> "astview.html")
  dir <- getDataDir
  rawSystem "firefox" [dir </> helpfile]
  return ()
    
-- | launches info dialog
actionAbout :: AstAction ()
actionAbout ref = do
  dlg <- fmap dlgAbout (getGui ref)
  aboutDialogSetUrlHook (\_ -> return ())
  licensefile <- getDataFileName ("data" </> "LICENSE.unwrapped")
  contents <- withFile licensefile ReadMode (fmap BS.unpack . BS.hGetContents)
  aboutDialogSetWrapLicense dlg True 
  aboutDialogSetLicense dlg (Just contents)
  widgetShow dlg

-- -------------------------------------------------------------------
-- ** other actions 
-- -------------------------------------------------------------------

-- | adds '*' to window title if file changed and sets state
actionBufferChanged :: AstAction ()
actionBufferChanged ref = do
  w <- fmap window (getGui ref)
  t <- windowGetTitle w 
  c <- getChanged ref
  when (not c) (windowSetTitle w ('*':t))
  setChanged True ref

-- | destroys window widget 
actionQuit :: AstAction ()
actionQuit ref = do 
  isChanged <- getChanged ref 
  when isChanged $ actionQuitWorker ref
  widgetDestroy =<< fmap window (getGui ref)


actionQuitWorker :: AstAction ()  
actionQuitWorker ref = do
      dia <- dialogNew
      dialogAddButton dia stockYes ResponseYes
      dialogAddButton dia stockNo ResponseNo
      dialogAddButton dia stockCancel ResponseCancel
      contain <- dialogGetUpper dia
  
      windowSetTitleSuffix dia "Quit" 
      containerSetBorderWidth dia 2
      file <- getFile ref
      lbl <- labelNew 
        (Just $ "Save changes to document \""++
                takeFileName file ++
                "\" before closing?")
      boxPackStartDefaults contain lbl

      widgetShowAll dia
      response <- dialogRun dia
      case response of 
        ResponseYes   -> actionSave ref
        _             -> return ()
      widgetHide dia


-- | launches open dialog
actionDlgOpenRun :: AstAction ()
actionDlgOpenRun ref = do
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
        \file -> actionLoadHeadless file ref
    _ -> return ()
  widgetHide dia

-- | launches save dialog
actionDlgSaveRun :: AstAction ()
actionDlgSaveRun ref = do
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
    ResponseOk     -> do
       maybeFile <- fileChooserGetFilename dia
       case maybeFile of
         Nothing-> return () 
         Just file -> do
            g <- getGui ref
            setChanged False ref
            setcFile file ref
            writeFile file =<< getText =<< getSourceBuffer ref
            windowSetTitle 
              (window g) 
              (takeFileName file)
    _ -> return ()
  widgetHide dia

-- |applies current parser to sourcebuffer 
actionReparse :: AstAction ()
actionReparse ref = 
  whenJustM (getLanguage ref) $ \l -> actionParse l ref >> return ()

-- |prints the current selected path to console
actionShowPath :: AstAction ()
actionShowPath ref = do
  p <- actionGetPath ref
  case p of
    []   -> return ()
    path -> print path

actionGetPath :: AstAction [Int]
actionGetPath ref = do 
  rows <- treeSelectionGetSelectedRows =<< treeViewGetSelection =<< getTreeView ref
  return $ case rows of 
    []    -> [] 
    (p:_) -> p

-- -------------------------------------------------------------------
-- ** Helpers
-- -------------------------------------------------------------------

-- |similar to @when@ 
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _       = return ()
whenJust (Just x) action = action x 
  
-- |similar to @whenJust@, but value is inside a monad
whenJustM :: Monad m => m(Maybe a) -> (a -> m ()) -> m ()
whenJustM val action = do
  m <- val
  when (isJust m) ((action.fromJust) m)  

-- |returns the text in given text buffer
getText :: TextBufferClass c => c -> IO String
getText tb = do
  start <- textBufferGetStartIter tb
  end <- textBufferGetEndIter tb
  textBufferGetText tb start end True

-- |uses the given string to set the title of given window with suffix "-astview"
windowSetTitleSuffix :: WindowClass w => w -> String -> IO ()
windowSetTitleSuffix win title = windowSetTitle win (title++" - astview")
  
-- |safe function to write files
writeFile :: FilePath -> String -> IO ()
writeFile f str = withFile f WriteMode (\h -> hPutStr h str >> hClose h)

