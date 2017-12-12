module Language.Astview.Gui.GtkActions where

import           Graphics.UI.Gtk hiding (Language)
import           Graphics.UI.Gtk.SourceView

import           Prelude                                         hiding (span,
                                                                  writeFile)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Char                  (toLower)
import           Data.Tree                  (Tree)
import           Control.Monad
import           System.FilePath                                 (takeFileName)
import           System.IO                                       (IOMode (..),
                                                                  hClose,
                                                                  hPutStr,
                                                                  withFile)

import           Language.Astview.Language
import           Language.Astview.Gui.Types


clearTreeView :: AstAction ()
clearTreeView = do
  t <- getTreeView
  liftIO $ do
    c <- treeViewGetColumn t 0
    case c of
      Just col-> treeViewRemoveColumn t col
      Nothing  -> return 0
  return ()

sourceViewSetText :: String -> AstAction ()
sourceViewSetText text = do
  sb <- getSourceBuffer
  liftIO $ textBufferSetText sb text

winSetTitle :: String -> AstAction ()
winSetTitle title = do
  w <- getWindow
  liftIO $ set w [windowTitle := title++" - astview" ]

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

-- |removes @*@ from window title if existing and updates state
deleteStar :: AstAction ()
deleteStar = do
  w <- getWindow
  bufferChanged <- getChanged
  liftIO $ do
    (t :: String) <- get w windowTitle
    when bufferChanged $
      set w [windowTitle := tail t]

-- |uses the name of given language to establish syntax highlighting in
-- source buffer
setupSyntaxHighlighting :: Language -> AstAction ()
setupSyntaxHighlighting language = do
  buffer <- getSourceBuffer
  liftIO $ do
    langManager <- sourceLanguageManagerGetDefault
    maybeLang <- sourceLanguageManagerGetLanguage
          langManager
          (map toLower $ syntax language)
    case maybeLang of
      Just lang -> do
        sourceBufferSetHighlightSyntax buffer True
        sourceBufferSetLanguage buffer (Just lang)
      Nothing -> sourceBufferSetHighlightSyntax buffer False


-- |select tree path
activatePath :: Path -> AstAction ()
activatePath p = do
  view <- getTreeView
  liftIO $ do
    treeViewExpandToPath view p
    treeViewExpandRow view p True
    treeViewSetCursor view p Nothing

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

treeviewSetTree :: Tree String -> AstAction (Tree String)
treeviewSetTree tree = do
  fontsize <- getFontsize
  view <- getTreeView
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

actionGetPath :: AstAction Path
actionGetPath = do
  tv <- getTreeView
  rows <- liftIO (treeSelectionGetSelectedRows =<< treeViewGetSelection tv)
  return $ case rows of
    []    -> []
    (p:_) -> p


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




-- * dialogs

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
    _              -> return ()
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
         Language.Astview.Gui.GtkActions.writeFile file =<< getText

-- | launches open dialog
actionDlgOpen :: (FilePath -> AstAction ()) -> AstAction ()
actionDlgOpen f = actionMkDialog FileChooserActionOpen onOkay where

  onOkay :: FileChooserDialog -> AstAction ()
  onOkay dia = whenJustM (liftIO $ fileChooserGetFilename dia) f

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

-- * Shutting down astview

-- |ends program with force
actionQuitForce :: AstAction ()
actionQuitForce = do
  w <- getWindow
  liftIO $ widgetDestroy w

actionQuitWorker :: AstAction () -> AstAction () -> AstAction ()
actionQuitWorker onYes onElse = do
  file <- getCurrentFile

  dialog <- liftIO $ messageDialogNew Nothing [] MessageQuestion ButtonsYesNo
    ("Save changes to document \""++takeFileName file ++ "\" before closing?")
  response <- liftIO $ do
    containerSetBorderWidth dialog 2
    widgetShowAll dialog
    dialogRun dialog
  case response of
    ResponseYes -> onYes
    _           -> onElse
  liftIO $ widgetHide dialog

-- * Helper functions

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



-- |safe function to write files
writeFile :: FilePath -> String -> AstAction ()
writeFile f str = liftIO $
  withFile f WriteMode (\h -> hPutStr h str >> hClose h)
