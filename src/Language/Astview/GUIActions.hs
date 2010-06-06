{- contains the GUIActions connected to menuItems
 -
 -}

module Language.Astview.GUIActions where

-- gui data types
import Language.Astview.GUIData 

-- base
import Prelude hiding (writeFile)
import Data.IORef
import Data.Maybe(fromJust,isJust)
import Data.List (find,findIndex)
import Control.Monad ((=<<),when)
import Data.Char (toLower)
-- io
import System.IO (withFile,IOMode(..),hPutStr,hClose)

-- filepath
import System.FilePath ((</>),takeExtension,takeFileName)
import System.Directory (doesDirectoryExist)

-- bytestring
import qualified Data.ByteString.Char8 as BS (hGetContents,unpack)

-- containers
import Data.Tree ( Tree(Node,rootLabel) )

-- gtk
import Graphics.UI.Gtk hiding (Language,get) 

-- gtksourceview
import Graphics.UI.Gtk.SourceView 

-- commands
import System.Cmd (rawSystem)

-- astview-utils
import Language.Astview.Language

-- generated on-the-fly by cabal
import Paths_astview (getDataFileName,getDataDir) 

-- |suffix of window title
suffix :: String
suffix = " - astview"

-- |unsaved document
unsavedDoc :: String
unsavedDoc = "Unsaved document"

-- | a list of pairs of gtk-ids and GUIActions 
menuActions :: [(String,AstAction ())]
menuActions = 
  [("mNew",actionEmptyGUI)
  ,("mOpenLeft",actionDlgOpenRun L)
  ,("mParseLeft",actionReparse L)
  ,("mParseRight",actionReparse R)
  ,("mOpenRight",actionDlgOpenRun R)
  ,("mParseAll",actionReparseAll)
  ,("mSaveLeft",actionSave L)
  ,("mSaveRight",actionSave R)
  ,("mPathLeft",actionShowPath L)
  ,("mPathRight",actionShowPath R)
  ,("mCut",actionCutSource)
  ,("mCopy",actionCopySource)
  ,("mPaste",actionPasteSource)
  ,("mDelete",actionDeleteSource)
  ,("mSrcLocLeft",actionJumpToSrcLoc L)
  ,("mSrcLocRight",actionJumpToSrcLoc R)
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
    Nothing -> return undefined
  return ()

-- | resets the GUI, 
actionEmptyGUI :: AstAction ()
actionEmptyGUI ref = do
  g <- getGui ref
  mapM_ clearTreeView =<< getTreeViews ref
  mapM_ (\s -> textBufferSetText s []) =<< getSourceBuffers ref
  windowSetTitle (window g) (unsavedDoc++suffix)  

-- | updates the sourceview with a given file, chooses a language by 
-- extension and parses the file
actionLoadHeadless :: Area -> FilePath -> AstAction ()
actionLoadHeadless area file ref = do
  sb <- getSourceBuffer area ref 
  tv <- getTreeView area ref
  setcFile area file ref
  s <- getAstState ref

  let langs = languages $ state s
  windowSetTitle 
    (window $ gui s) 
    (takeFileName file ++ suffix)
  catch 
    (do
      contents <- withFile 
        file ReadMode (fmap BS.unpack . BS.hGetContents)
      textBufferSetText sb contents
      deleteStar ref
    )
    print
  whenJust 
    (find (elem (takeExtension file) . exts) langs) $
    \l -> activateLang l ref 
  l <- getcLang ref
  actionParse area l ref 
  return ()

-- | helper for loadHeadless
activateLang :: Language -> AstAction ()
activateLang l ref = do
  setLanguage l ref
  langs <- getLangs ref 
  whenJust 
    (findIndex (l==) langs) $
    \i -> do
      combobox <- getcBox ref
      comboBoxSetActive combobox  i

-- | parses the contents of the sourceview with the selected language
actionParse :: Area -> Language -> AstAction (Tree String)
actionParse a l@(Language _ _ _ p to _ _) ref = do
  setLanguage l ref
  g <- getGui ref
  sb <- getSourceBuffer a ref
  tv <- getTreeView a ref
  sourceBufferSetHighlightSyntax sb True
  setupSyntaxHighlighting sb l g
  plain <- getText sb g
  clearTreeView tv
  let eitherTree = fmap to (p plain)

  -- error handling
  case eitherTree of
    Left (ErrLocation (SrcLocation l r) m) -> do 
      iter <- textBufferGetStartIter sb
      textIterSetLine iter (l-1)
      textBufferPlaceCursor sb iter
    _ -> return ()

  let t = case eitherTree of
          Right ast                 -> ast
          Left Err                  -> Node "Parse error" []
          Left (ErrMessage m)       -> Node m []
          Left (ErrLocation (SrcLocation l r) m) -> 
            Node ("Parse error at:"++show l ++":"++show r) [] 
  
  model <- treeStoreNew [t]
  treeViewSetModel tv model
  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True 
  cellLayoutSetAttributes 
    col 
    renderer 
    model 
    (\row -> [ cellText := row ] )
  treeViewAppendColumn tv col 
  return t
  where 
    setupSyntaxHighlighting :: SourceBuffer -> Language -> GUI -> IO ()
    setupSyntaxHighlighting sb l g = do
      langManager <- sourceLanguageManagerGetDefault
      maybeLang <- sourceLanguageManagerGetLanguage 
        langManager 
        (map toLower $ syntax l)
      case maybeLang of
        Just l -> do
          sourceBufferSetHighlightSyntax sb True
          sourceBufferSetLanguage sb l 
        Nothing ->
          sourceBufferSetHighlightSyntax sb False   

-- |saves a file 
actionSave :: Area -> AstAction ()
actionSave a ref = do
  url <- getFile a ref
  sb <- getSourceBuffer a ref
  text <- getText sb =<< getGui ref
  actionSaveWorker text url ref

-- |saves current file if a file is active or calls "save as"-dialog
actionSaveWorker :: String -> FilePath -> AstAction ()
actionSaveWorker plain file ref = 
  case file of
    "Unsaved document"  -> actionDlgSaveRun ref
    otherwise           -> do 
      deleteStar ref 
      writeFile file plain 

-- |removes @*@ from window title if existing and updates state
deleteStar :: AstAction ()
deleteStar ref = do
  w <- getWindow ref
  t <- windowGetTitle w
  a <- getCArea ref
  setChanged a False ref
  when (head t == '*') 
    (windowSetTitle w (tail t))
 
-- -------------------------------------------------------------------
-- ** editmenu menu actions
-- -------------------------------------------------------------------

-- |moves selected source to clipboard (cut)
actionCutSource :: AstAction ()  
actionCutSource ref = do
  actionCopySource ref
  actionDeleteSource ref

-- |copies selected source to clipboard  
actionCopySource :: AstAction () 
actionCopySource ref = do
  sb <- getcSourceBuffer ref
  gui <- getGui ref
  (start,end) <- textBufferGetSelectionBounds sb 
  clipBoard <- clipboardGet selectionClipboard
  clipboardSetText 
    clipBoard 
    =<< textBufferGetText sb start end True

-- |pastes text from clipboard at current cursor position  
actionPasteSource :: AstAction ()
actionPasteSource ref = do 
  sb <- getcSourceBuffer ref
  gui <- getGui ref
  clipBoard <- clipboardGet selectionClipboard
  clipboardRequestText clipBoard (insertAt sb) where
    insertAt :: SourceBuffer -> Maybe String -> IO ()
    insertAt tb m = whenJust m (textBufferInsertAtCursor tb)

-- |deletes selected source
actionDeleteSource :: AstAction ()
actionDeleteSource ref = do 
  sb <- getcSourceBuffer ref
  gui <- getGui ref
  textBufferDeleteSelection sb False False >> return ()

-- |jumps to the node in tree given by current cursor position. If
-- cursor position does not match any source location in tree we 
-- will jump to a source location of the correc line (if existing)
actionJumpToSrcLoc :: Area -> AstAction ()
actionJumpToSrcLoc a ref = do  
  tv <- getTreeView a ref
  gui <- getGui ref
  -- get cursor position
  -- zero point: line 1, row 0
  (iter,_) <- textBufferGetSelectionBounds =<< getSourceBuffer a ref
  l <- textIterGetLine iter
  r <- textIterGetLineOffset iter
  
  -- reparse and set cursor in treeview
  lang <- getcLang ref 
  t <- actionParse a lang ref 
  let sl = sourceLocations lang t
  let setCursor p = do 
      treeViewExpandToPath tv p
      treeViewSetCursor tv p Nothing
  case find (\(SrcLocation x y,_) ->(l==x &&r==y)) sl of
    Just (_,p) -> setCursor p
    Nothing      -> 
      -- jump to src loc of given line if no exact matching found
      case find (\(SrcLocation x _,_) ->l==x) sl of
        Just (_,p) -> setCursor p
        Nothing -> return ()
  
-- |returns all source locations and paths to source
-- locations in current tree
sourceLocations :: Language -> Tree String -> [(SrcLocation,TreePath)]
sourceLocations l = getSourceLocations l . calcPaths [0]
  where
  calcPaths :: [Int] -> Tree String -> Tree (String,TreePath)
  calcPaths p (Node l cs) = 
    let paths = zipWith (\p e->p++[e]) (repeat p) [0,1..] in
    Node (l,p) (zipWith (\subtree p -> calcPaths p subtree) cs paths)

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
  gui <- getGui ref
  aboutDialogSetUrlHook (\_ -> return ())
  licensefile <- getDataFileName ("data" </> "LICENSE.unwrapped")
  contents <- catch 
    (withFile licensefile ReadMode (fmap BS.unpack . BS.hGetContents))
    (\ioe -> return $ "Err" ++ show ioe)
  aboutDialogSetWrapLicense (dlgAbout gui) True 
  aboutDialogSetLicense (dlgAbout gui) (Just contents)
  widgetShow (dlgAbout gui)

-- -------------------------------------------------------------------
-- ** other actions 
-- -------------------------------------------------------------------

-- | adds '*' to window title if file changed and sets state
actionBufferChanged :: Area -> AstAction ()
actionBufferChanged area ref = do
  gui <- getGui ref
  setcArea area ref 
  setChanged area True ref
  t <- windowGetTitle (window gui)
  when (head t /= '*') (windowSetTitle (window gui) ('*':t))

-- | destroys window widget 
actionQuit :: AstAction ()
actionQuit ref = do 
  changedL <- getChanged L ref 
  changedR <- getChanged R ref 
  gui <- getGui ref
  when changedL $ actionQuitWorker L ref
  when changedR $ actionQuitWorker R ref
  widgetDestroy (window gui)

actionQuitWorker :: Area -> AstAction ()  
actionQuitWorker a ref = do
      dia <- dialogNew
      dialogAddButton dia stockYes ResponseYes
      dialogAddButton dia stockNo ResponseNo
      dialogAddButton dia stockCancel ResponseCancel
      contain <- dialogGetUpper dia
  
      windowSetTitle dia "astview"
      containerSetBorderWidth dia 2
      file <- getFile a ref
      lbl <- labelNew 
        (Just $ "Save changes to document \""++
                takeFileName file ++
                "\" before closing?")
      boxPackStartDefaults contain lbl

      widgetShowAll dia
      response <- dialogRun dia
      gui <- getGui ref
      case response of 
        ResponseYes   -> actionSave a ref
        _             -> return ()
      widgetHide dia


-- | launches open dialog
actionDlgOpenRun :: Area -> AstAction ()
actionDlgOpenRun a ref = do
  gui <- getGui ref 
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
        \file -> actionLoadHeadless a file ref
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
            a <- getCArea ref
            setcFile a file ref
            setChanged a False ref
            sb <- getcSourceBuffer ref
            writeFile file =<< getText sb g
            windowSetTitle 
              (window g) 
              (takeFileName file++suffix)
    _ -> return ()
  widgetHide dia

actionReparseAll :: AstAction ()
actionReparseAll ref = actionReparse L ref >> actionReparse R ref


-- |applies current parser to current sourcebuffer 
actionReparse :: Area -> AstAction ()
actionReparse a ref = do
  l <- getcLang ref
  actionParse a l ref
  activateLang l ref

data Direction 
  = D -- ^ go down one level to the leftmost child
  | Ri -- ^ stay at the same level and go to the right
  deriving Show


actionShowPath a ref = do
  p <- actionGetPath a ref
  if null p 
    then return () 
    else print (tail p)

actionGetPath :: Area -> AstAction [Direction]
actionGetPath a ref = do 
  gui <- getGui ref
  s <- treeSelectionGetSelectedRows 
    =<< treeViewGetSelection =<< getTreeView a ref
  if null s
    then return []
    else return $ trans (head s) where
      -- transforms gtk2hs path representation to direction
      trans :: [Int] -> [Direction]
      trans (x:xs) = D : replicate x Ri ++ trans xs
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
getText :: SourceBuffer -> GUI -> IO String
getText sb gui = do
  start <- textBufferGetStartIter sb
  end <- textBufferGetEndIter sb
  textBufferGetText sb start end True
  
-- |safe function to write files
writeFile :: FilePath -> String -> IO ()
writeFile f str = catch
  (withFile f WriteMode (\h -> hPutStr h str >> hClose h))
  print

