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
  ,("mOpen",actionDlgOpenRun)
  ,("mParse",actionReparse)
  ,("mPath",actionGetPath)
  ,("mSave",actionSave)
  ,("mSaveAs",actionDlgSaveRun)
  ,("mCut",actionCutSource)
  ,("mCopy",actionCopySource)
  ,("mPaste",actionPasteSource)
  ,("mDelete",actionDeleteSource)
  ,("mSrcLoc",actionJumpToSrcLoc)
  ,("mAbout",actionAbout)
  ,("mShowHelp",actionHelp)
  ,("mQuit",actionQuit)
  ]


-- -------------------------------------------------------------------
-- * filemenu menu actions
-- -------------------------------------------------------------------

-- | resets the GUI, 
actionEmptyGUI :: AstAction ()
actionEmptyGUI ref = do
  g <- getGui ref
  maybeCol <- treeViewGetColumn (tv g) 0 
  case maybeCol of
    Just col-> treeViewRemoveColumn (tv g) col
    Nothing -> return undefined
  textBufferSetText (tb g) ""
  windowSetTitle (window g) (unsavedDoc++suffix)  

-- | updates the sourceview with a given file, chooses a language by 
-- extension and parses the file
actionLoadHeadless :: FilePath -> AstAction () 
actionLoadHeadless file ref = do
  setcFile file ref
  s <- getAstState ref

  let langs = languages $ state s
  windowSetTitle 
    (window $ gui s) 
    (takeFileName file ++ suffix)
  catch 
    (do
      contents <- withFile 
        file ReadMode (fmap BS.unpack . BS.hGetContents)
      textBufferSetText (tb $ gui s) contents
      deleteStar ref
    )
    print
  case find (elem (takeExtension file) . exts) langs of
    Just l -> do 
            actionParse l ref
            activateLang l ref 
    Nothing -> return ()
  
-- | helper for loadHeadless
activateLang :: Language -> AstAction ()
activateLang l ref = do
  setLanguage l ref
  langs <- getLangs ref 
  case findIndex (l==) langs of
    Just i -> do
      combobox <- getcBox ref
      comboBoxSetActive combobox  i
    Nothing-> return ()

-- | parses the contents of the sourceview with the selected language
actionParse :: Language -> AstAction (Tree String)
actionParse l@(Language _ _ _ p to _ _) ref = do
  setLanguage l ref
  g <- getGui ref
  sourceBufferSetHighlightSyntax (tb g) True
  setupSyntaxHighlighting l g
  plain <- getText g
  maybeCol <- treeViewGetColumn (tv g) 0
  case maybeCol of
    Just col-> treeViewRemoveColumn (tv g) col
    Nothing -> return (-1)
  
  let eitherTree = fmap to (p plain)

  -- error handling
  case eitherTree of
    Left (ErrLocation (SrcLocation l r) m) -> do 
      iter <- textBufferGetStartIter (tb g)
      textIterSetLine iter (l-1)
      textBufferPlaceCursor (tb g) iter
    _ -> return ()

  let t = case p plain of
          Right ast -> to ast
          Left Err                  -> Node "Parse error" []
          Left (ErrMessage m)       -> Node m []
          Left (ErrLocation (SrcLocation l r) m) -> 
            Node ("Parse error at:"++show l ++":"++show r) [] 
  
  setcTree t ref

  model <- treeStoreNew [t]
  treeViewSetModel (tv g) model
  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True 
  cellLayoutSetAttributes 
    col 
    renderer 
    model 
    (\row -> [ cellText := row ] )
  treeViewAppendColumn (tv g) col 
  return t
  where 
    setupSyntaxHighlighting :: Language -> GUI -> IO ()
    setupSyntaxHighlighting l g = do
      langManager <- sourceLanguageManagerGetDefault
      maybeLang <- sourceLanguageManagerGetLanguage 
        langManager 
        (map toLower $ syntax l)
      case maybeLang of
        Just l -> do
          sourceBufferSetHighlightSyntax (tb g) True
          sourceBufferSetLanguage (tb g) l 
        Nothing-> 
          sourceBufferSetHighlightSyntax (tb g) False   

-- |saves a file 
actionSave :: AstAction ()
actionSave ref = do
  g <- getGui ref
  url <- getcFile ref
  text <- getText g
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
  setChanged False ref
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
  gui <- getGui ref
  (start,end) <- textBufferGetSelectionBounds (tb gui) 
  clipBoard <- clipboardGet selectionClipboard
  clipboardSetText 
    clipBoard 
    =<< textBufferGetText (tb gui) start end True

-- |pastes text from clipboard at current cursor position  
actionPasteSource :: AstAction ()
actionPasteSource ref = do
  gui <- getGui ref
  clipBoard <- clipboardGet selectionClipboard
  clipboardRequestText clipBoard (insertAt (tb gui)) where
    insertAt :: SourceBuffer -> Maybe String -> IO ()
    insertAt tb m = whenJust m (textBufferInsertAtCursor tb)

-- |deletes selected source
actionDeleteSource :: AstAction ()
actionDeleteSource ref = do 
  gui <- getGui ref
  textBufferDeleteSelection (tb gui) False False >> return ()

-- |jumps to the node in tree given by current cursor position. If
-- cursor position does not match any source location in tree we 
-- will jump to a source location of the correc line (if existing)
actionJumpToSrcLoc :: AstAction ()
actionJumpToSrcLoc ref = do
  gui <- getGui ref
  -- get cursor position
  -- zero point: line 1, row 0
  (iter,_) <- textBufferGetSelectionBounds (tb gui)
  l1 <- textIterGetLine iter
  r <- textIterGetLineOffset iter
  let l = l1+1
  
  -- reparse and set cursor in treeview
  lang <- getcLang ref 
  t <- actionParse lang ref 
  let sl = sourceLocations t
  let setCursor p = do 
      treeViewExpandToPath (tv gui) p
      treeViewSetCursor (tv gui) p Nothing
  case find (\(x,y,_) ->(l==x &&r==y)) sl of
    Just (_,_,p) -> setCursor p
    Nothing      -> 
      -- jump to src loc of given line if no exact matching found
      case find (\(x,_,_) ->l==x) sl of
        Just (_,_,p) -> setCursor p
        Nothing -> return ()
  
-- |returns all source locations and paths to source
-- locations in current tree
sourceLocations :: Tree String -> [(Int,Int,TreePath)]
sourceLocations = getSourceLocations . calcPaths [0]
  where
  calcPaths :: [Int] -> Tree String -> Tree (String,TreePath)
  calcPaths p (Node l cs) = 
    let paths = zipWith (\p e->p++[e]) (repeat p) [0,1..] in
    Node (l,p) (zipWith (\subtree p -> calcPaths p subtree) cs paths)

  getSourceLocations :: Tree (String,TreePath) -> [(Int,Int,TreePath)]
  getSourceLocations (Node ("SrcLoc",p) cs) =
    [(1+read (to 1)::Int,(read (to 2):: Int)-1,p)] 
    where to = rootLabel . fmap fst . (cs !!)
  getSourceLocations (Node _ cs) = concatMap getSourceLocations cs

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
actionBufferChanged :: AstAction ()
actionBufferChanged ref = do
  gui <- getGui ref
  setChanged True ref
  t <- windowGetTitle (window gui)
  when (head t /= '*') (windowSetTitle (window gui) ('*':t))


-- | destroys window widget 
actionQuit :: AstAction ()
actionQuit ref = do 
  changed <- getChanged ref 
  gui <- getGui ref
  if changed 
    then do 
      dia <- dialogNew
      dialogAddButton dia stockYes ResponseYes
      dialogAddButton dia stockNo ResponseNo
      dialogAddButton dia stockCancel ResponseCancel
      contain <- dialogGetUpper dia
  
      windowSetTitle dia "astview"
      containerSetBorderWidth dia 2
      file <- getcFile ref
      lbl <- labelNew 
        (Just $ "Save changes to document \""++
                takeFileName file ++
                "\" before closing?")
      boxPackStartDefaults contain lbl

      widgetShowAll dia
      response <- dialogRun dia
      case response of 
        ResponseYes    -> actionSave ref>>widgetDestroy (window gui)
        ResponseCancel -> return ()
        ResponseNo     -> widgetDestroy (window gui)
      widgetHide dia
    else widgetDestroy (window gui)
  

-- | launches open dialog
actionDlgOpenRun :: AstAction ()
actionDlgOpenRun ref = do
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
        \file -> actionLoadHeadless file ref
  widgetHide dia

-- | launches save dialog
actionDlgSaveRun :: AstAction ()
actionDlgSaveRun ref = do
  g <- getGui ref
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
            setcFile file ref
            setChanged False ref
            writeFile file =<< getText g
            windowSetTitle 
              (window g) 
              (takeFileName file++suffix)
  widgetHide dia

-- |applies current parser to current sourcebuffer 
actionReparse :: AstAction ()
actionReparse ref = do
  l <- getcLang ref
  activateLang l ref
  actionParse l ref
  return ()

data Direction 
  = D -- ^ go down one level to the leftmost child
  | R -- ^ stay at the same level and go to the right
  deriving Show

actionGetPath :: AstAction () 
actionGetPath ref = do
  gui <- getGui ref
  s <- treeViewGetSelection $ tv gui
  t <- fmap (tail.head) $ treeSelectionGetSelectedRows s
  putStr $ show t ++ " : " 
  print $ trans t where
    -- transforms gtk2hs path representation to direction
    trans :: [Int] -> [Direction]
    trans (x:xs) = D : replicate x R ++ trans xs
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
  print

