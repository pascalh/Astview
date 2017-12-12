{- contains the main gui functions
 -}

module Language.Astview.Gui.Actions where
import Language.Astview.Gui.Types
import Language.Astview.Language
import Language.Astview.SmallestSrcLocContainingCursor
  (smallestSrcLocContainingCursorPos)
import Language.Astview.DataTree(flatten)


import Prelude hiding (span,writeFile)
import Data.List (find)
import Control.Monad (when,unless,void,zipWithM_)
import Control.Monad.IO.Class(liftIO)
import Data.Char (toLower)
import System.IO (withFile,IOMode(..),hPutStr,hClose)
import System.FilePath (takeExtension,takeFileName)
import qualified Data.ByteString.Char8 as BS (hGetContents,unpack)
import Data.Tree ( Tree(Node) )
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative((<$>))
#endif

import Graphics.UI.Gtk hiding (Language,response,bufferChanged)
import Graphics.UI.Gtk.SourceView

-- -------------------------------------------------------------------
-- * filemenu menu actions
-- -------------------------------------------------------------------

clearTreeView :: AstAction ()
clearTreeView = do
  t <- getTreeView
  liftIO $ do
    c <- treeViewGetColumn t 0
    case c of
      Just col-> treeViewRemoveColumn t col
      Nothing -> return 0
  return ()

-- | resets the GUI,
actionEmptyGUI :: AstAction ()
actionEmptyGUI = do
  g <- getGui
  sb <- getSourceBuffer
  clearTreeView
  liftIO $ do
    flip textBufferSetText ("" :: String) sb
    windowSetTitleSuffix (window g) unsavedDoc

-- | updates the sourceview with a given file and parses the file
actionLoadHeadless :: FilePath -> AstAction ()
actionLoadHeadless file = do
  setCurrentFile file
  w <- getWindow
  liftIO $ windowSetTitleSuffix w (takeFileName file)
  buffer <- getSourceBuffer
  liftIO $ textBufferSetText buffer =<< withFile file ReadMode (fmap BS.unpack . BS.hGetContents)
  deleteStar
  actionReparse

-- |tries to find a language based on the extension of
-- current file name
getLanguageByExtension :: AstAction (Maybe Language)
getLanguageByExtension = do
  file <- getCurrentFile
  languages <- getKnownLanguages
  return $ find (elem (takeExtension file) . exts) languages

getLanguage :: AstAction (Maybe Language)
getLanguage = do
  maybeLang <- getActiveLanguage
  case maybeLang of
    Nothing   -> getLanguageByExtension
    Just lang -> return $ Just lang

actionGetAst :: Language -> AstAction (Either Error Ast)
actionGetAst l = do
  plain <- getText
  flattening <- getFlattenLists
  return $ (if flattening then flatten else id) <$> parse l plain

-- | parses the contents of the sourceview with the selected language
actionParse :: Language -> AstAction (Tree String)
actionParse l = do
  buffer <- getSourceBuffer
  view <- getTreeView
  liftIO $ do
    sourceBufferSetHighlightSyntax buffer True
    setupSyntaxHighlighting buffer l
  tree <- buildTree <$> actionGetAst l
  clearTreeView
  fontsize <- getFontsize
  liftIO $ do
    model <- treeStoreNew [tree]
    treeViewSetModel view model
    col <- treeViewColumnNew
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes
      col
      renderer
      model
      (\row -> [ cellText := row
               , cellTextSize := (fromInteger . toInteger) fontsize
               ] )
    treeViewAppendColumn view col
  return tree

-- |constructs the tree which will be presented by our gtk-treeview
buildTree :: Either Error Ast  -> Tree String
buildTree (Left Err)                 = Node "Parse error" []
buildTree (Left (ErrMessage m))      = Node m []
buildTree (Left (ErrLocation pos m)) = Node ("Parse error at:"++show pos++": "++m) []
buildTree (Right t)                  = label <$> ast t

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
actionSave = do
  file <- getCurrentFile
  text <- getText
  case file of
    "Unsaved document"  -> actionDlgSave
    _                   -> do
      deleteStar
      writeFile file text

-- |sets up a simple filechooser dialog, whose response to Ok
-- is given by argument function
actionMkDialog :: FileChooserAction -> (FileChooserDialog  -> AstAction ()) -> AstAction()
actionMkDialog fileChooser actionOnOkay = do
  dia <- liftIO $ fileChooserDialogNew
    (Just ("astview" :: String))
    Nothing
    fileChooser
    []

  liftIO $ zipWithM_ (dialogAddButton dia)
    [stockCancel,stockOpen] [ResponseCancel,ResponseOk]

  liftIO $ widgetShowAll dia
  response <- liftIO $ dialogRun dia
  case response of
    ResponseCancel -> return ()
    ResponseOk     -> actionOnOkay dia
    _ -> return ()
  liftIO $ widgetHide dia

-- |lanches the "save as"-dialog
actionSaveAs :: AstAction ()
actionSaveAs = actionMkDialog FileChooserActionSave onOkay where

  onOkay :: FileChooserDialog -> AstAction ()
  onOkay dia = do
    maybeFile <- liftIO $ fileChooserGetFilename dia
    case maybeFile of
       Nothing-> return ()
       Just file -> do
         setCurrentFile file
         writeFile file =<< getText

-- |removes @*@ from window title if existing and updates state
deleteStar :: AstAction ()
deleteStar = do
  w <- getWindow
  bufferChanged <- getChanged
  liftIO $ do
    (t :: String) <- get w windowTitle
    when bufferChanged $
      set w [windowTitle := tail t]
  setChanged False

-- -------------------------------------------------------------------
-- ** editmenu menu actions
-- -------------------------------------------------------------------

-- |moves selected source to clipboard (cut)
actionCutSource :: AstAction ()
actionCutSource = do
  actionCopySource
  actionDeleteSource

-- |copies selected source to clipboard
actionCopySource :: AstAction ()
actionCopySource = do
  buffer <- getSourceBuffer
  liftIO $ do
    (start,end) <- textBufferGetSelectionBounds buffer
    clipBoard <- clipboardGet selectionClipboard
    s :: String <- textBufferGetText buffer start end True
    clipboardSetText clipBoard s

-- |pastes text from clipboard at current cursor position
actionPasteSource :: AstAction ()
actionPasteSource = do
  buffer <- getSourceBuffer
  liftIO $ do
    clipBoard <- clipboardGet selectionClipboard
    clipboardRequestText clipBoard (insertAt buffer) where

      insertAt :: SourceBuffer -> Maybe String -> IO ()
      insertAt buff m = whenJust m (textBufferInsertAtCursor buff)

-- |deletes selected source
actionDeleteSource :: AstAction ()
actionDeleteSource = void $ do
  buffer <- getSourceBuffer
  liftIO $ textBufferDeleteSelection buffer False False

-- |launches a dialog which displays the text position associated to
-- last clicked tree node.
actionJumpToTextLoc :: AstAction ()
actionJumpToTextLoc = do
  maybeLang <- getLanguage
  case maybeLang of
    Nothing -> return ()
    Just lang -> do
      astOrError <- actionGetAst lang
      case astOrError of
        Left _    -> return ()
        Right (Ast ast) -> do
          gtkPath <- getPath
          unless (null gtkPath) $ do
            let astPath = tail gtkPath
                loc = ast `at` astPath
            case loc of
              Nothing -> return ()
              Just l  -> actionSelectSrcLoc l

-- |selects the given source location in gui textview
actionSelectSrcLoc :: SrcSpan -> AstAction ()
actionSelectSrcLoc (SrcSpan (SrcPos bl br)  (SrcPos el er)) = do
  textBuffer <- getSourceBuffer
  liftIO $ do
    let getIter line row = textBufferGetIterAtLineOffset textBuffer (line-1) (0 `max` row-1)
    -- we need to subtract 1 since lines and offsets start with 0
    begin <- getIter bl br
    end <- getIter el er
    textBufferSelectRange textBuffer begin end

at :: Tree AstNode -> Path -> Maybe SrcSpan
at (Node n _ )  []     = srcspan n
at (Node _ cs) (i:is)  = get i cs >>= \tree -> tree `at` is where

  get :: Int -> [a] -> Maybe a
  get _ [] = Nothing
  get n (x:xs)
    | n <  0    = Nothing
    | n >  0    = get (n-1) xs
    | otherwise = Just x


-- |returns the current cursor position in a source view.
-- return type: (line,row)
getCursorPosition :: AstAction SrcSpan
getCursorPosition = do
  buffer <- getSourceBuffer
  liftIO $ do
    (startIter,endIter) <- textBufferGetSelectionBounds buffer
    lineStart <- textIterGetLine startIter
    rowStart <- textIterGetLineOffset startIter
    lineEnd <- textIterGetLine endIter
    rowEnd <- textIterGetLineOffset endIter
    return $ span (lineStart+1) (rowStart+1) (lineEnd+1) (rowEnd+1)

-- |opens tree position associated with current cursor position.
actionJumpToSrcLoc :: AstAction ()
actionJumpToSrcLoc = do
  treePath <- actionGetAssociatedPath
  case treePath of
    Just p  -> activatePath p
    Nothing -> return ()

-- |returns the shortest path in tree which is associated with the
-- current selected source location.
actionGetAssociatedPath :: AstAction (Maybe Path)
actionGetAssociatedPath = do
  sele <- getCursorPosition
  maybeLang <- getLanguage
  case maybeLang of
    Nothing -> return Nothing
    Just lang -> do
      astOrError <- actionGetAst lang
      case astOrError of
        Left _    -> return Nothing
        Right ast ->
           return $ smallestSrcLocContainingCursorPos sele ast


-- |select tree path
activatePath :: Path -> AstAction ()
activatePath p = do
  view <- getTreeView
  liftIO $ do
    treeViewExpandToPath view p
    treeViewExpandRow view p True
    treeViewSetCursor view p Nothing

-- -------------------------------------------------------------------
-- ** other actions
-- -------------------------------------------------------------------

-- | adds '*' to window title if file changed and sets state
actionBufferChanged :: AstAction ()
actionBufferChanged = do
  w <- fmap window getGui
  t <- liftIO $ get w windowTitle
  c <- getChanged
  unless c $ liftIO $ set w [windowTitle := '*':t]
  cp <- getCursorPosition
  setCursor cp
  setChanged True

-- | destroys window widget
actionQuit :: AstAction ()
actionQuit = do
  isChanged <- getChanged
  when isChanged $ actionQuitWorker
  actionQuitForce

-- |ends program with force
actionQuitForce :: AstAction ()
actionQuitForce = do
  w <- getWindow
  liftIO $ widgetDestroy w

actionQuitWorker :: AstAction ()
actionQuitWorker = do
  file <- getCurrentFile

  dialog <- liftIO $ messageDialogNew Nothing [] MessageQuestion ButtonsYesNo
    ("Save changes to document \""++takeFileName file ++ "\" before closing?")
  response <- liftIO $ do
    containerSetBorderWidth dialog 2
    widgetShowAll dialog
    dialogRun dialog
  case response of
    ResponseYes   -> actionSave
    _             -> actionQuitForce
  liftIO $ widgetHide dialog


-- | launches open dialog
actionDlgOpen :: AstAction ()
actionDlgOpen = actionMkDialog FileChooserActionOpen onOkay where

  onOkay :: FileChooserDialog -> AstAction ()
  onOkay dia = whenJustM (liftIO $ fileChooserGetFilename dia) actionLoadHeadless

-- | launches save dialog
actionDlgSave :: AstAction ()
actionDlgSave = actionMkDialog FileChooserActionSave onOkay where

  onOkay :: FileChooserDialog -> AstAction ()
  onOkay dia = do
     maybeFile <- liftIO $ fileChooserGetFilename dia
     case maybeFile of
       Nothing-> return ()
       Just file -> do
          g <- getGui
          setChanged False
          setCurrentFile file
          writeFile file =<< getText
          liftIO $ set (window g) [windowTitle := takeFileName file]

-- |applies current parser to sourcebuffer
actionReparse :: AstAction ()
actionReparse =
  whenJustM getLanguage (void . actionParse)

actionGetPath :: AstAction Path
actionGetPath = do
  tv <- getTreeView
  rows <- liftIO (treeSelectionGetSelectedRows =<< treeViewGetSelection tv)
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
  whenJust m action

-- |returns the text in given text buffer
getText :: AstAction String
getText = do
  tb <- getSourceBuffer
  liftIO $ do
    start <- textBufferGetStartIter tb
    end <- textBufferGetEndIter tb
    textBufferGetText tb start end True

-- |uses the given string to set the title of given window with
-- suffix "-astview". Window titles should only be set by this
-- function, hence it replaces the corresponding gtk function.
windowSetTitleSuffix :: WindowClass w => w -> String -> IO ()
windowSetTitleSuffix win title = set win [windowTitle := title++" - astview" ]

-- |safe function to write files
writeFile :: FilePath -> String -> AstAction ()
writeFile f str = liftIO $
  withFile f WriteMode (\h -> hPutStr h str >> hClose h)
