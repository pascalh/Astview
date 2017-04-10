{-#  LANGUAGE FlexibleContexts #-}

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

import Control.Monad.Free
{-
http://dlaing.org/cofun/posts/free_and_cofree.html

-}

data TextBufferInstruction
 = Clear
 | SetText String

data TextBufferAction next
  = TextBufferAction TextBufferInstruction next
  | TextBufferGetSelection ((Int,Int) -> next)

textbufferclear :: Free TextBufferAction ()
textbufferclear = liftF $ (TextBufferAction Clear) ()

textbuffersettext :: String -> Free TextBufferAction ()
textbuffersettext text = liftF $ (TextBufferAction $ SetText text) ()

textbuffergetselection :: Free TextBufferAction (Int,Int)
textbuffergetselection = liftF $ TextBufferGetSelection id


interText :: Free TextBufferAction r -> IO ()
interText (Pure _) = return ()
interText (Free (TextBufferAction act next)) = case act of
  Clear -> putStrLn "CLEAR" >> interText next
  SetText str ->putStrLn str >> interText next
interText (Free (TextBufferGetSelection next)) =
  interText  $ next(1,1)

textseeqqq :: Free TextBufferAction ()
textseeqqq = do
  textbuffersettext "ab"
  (b,_) <- textbuffergetselection
  textbuffersettext $ show b
  textbufferclear

{-TODO
1) how to carry state in free?
2) translate free to astaction
-}

toAstAction :: Free TextBufferAction r -> AstAction r
toAstAction _ = undefined

instance Functor TextBufferAction where
  fmap f (TextBufferAction act next) = TextBufferAction act $ f next
  fmap f (TextBufferGetSelection g) = TextBufferGetSelection (f . g)

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
  flip textBufferSetText ("" :: String) =<< getSourceBuffer ref
  windowSetTitleSuffix (window g) unsavedDoc

-- | updates the sourceview with a given file and parses the file
actionLoadHeadless :: FilePath -> AstAction ()
actionLoadHeadless file ref = do
  setCurrentFile file ref
  w <- getWindow ref
  windowSetTitleSuffix w (takeFileName file)
  buffer <- getSourceBuffer ref
  textBufferSetText buffer =<< withFile file ReadMode (fmap BS.unpack . BS.hGetContents)
  deleteStar ref
  actionReparse ref

-- |tries to find a language based on the extension of
-- current file name
getLanguageByExtension :: AstAction (Maybe Language)
getLanguageByExtension ref = do
  file <- getCurrentFile ref
  languages <- getKnownLanguages ref
  return $ find (elem (takeExtension file) . exts) languages

getLanguage :: AstAction (Maybe Language)
getLanguage ref = do
  maybeLang <- getActiveLanguage ref
  case maybeLang of
    Nothing   -> getLanguageByExtension ref
    Just lang -> return $ Just lang

actionGetAst :: Language -> AstAction (Either Error Ast)
actionGetAst l ref = do
  plain <- getText =<< getSourceBuffer ref
  flattening <- getFlattenLists ref
  return $ (if flattening then flatten else id) <$> parse l plain

-- | parses the contents of the sourceview with the selected language
actionParse :: Language -> AstAction (Tree String)
actionParse l ref = do
  buffer <- getSourceBuffer ref
  view <- getTreeView ref
  sourceBufferSetHighlightSyntax buffer True
  setupSyntaxHighlighting buffer l
  tree <- buildTree <$> actionGetAst l ref
  clearTreeView view
  model <- treeStoreNew [tree]
  treeViewSetModel view model
  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True
  fontsize <- getFontsize ref
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
actionSave ref = do
  file <- getCurrentFile ref
  text <- getText =<< getSourceBuffer ref
  case file of
    "Unsaved document"  -> actionDlgSave ref
    _                   -> do
      deleteStar ref
      writeFile file text

-- |sets up a simple filechooser dialog, whose response to Ok
-- is given by argument function
actionMkDialog :: FileChooserAction -> (FileChooserDialog  -> t -> IO ()) -> t -> IO()
actionMkDialog fileChooser actionOnOkay ref = do
  dia <- fileChooserDialogNew
    (Just ("astview" :: String))
    Nothing
    fileChooser
    []

  zipWithM_ (dialogAddButton dia) [stockCancel   ,stockOpen]
                                  [ResponseCancel,ResponseOk]

  widgetShowAll dia
  response <- dialogRun dia
  case response of
    ResponseCancel -> return ()
    ResponseOk     -> actionOnOkay dia ref
    _ -> return ()
  widgetHide dia

-- |lanches the "save as"-dialog
actionSaveAs :: AstAction ()
actionSaveAs = actionMkDialog FileChooserActionSave onOkay where
  onOkay dia ref = do
    maybeFile <- fileChooserGetFilename dia
    case maybeFile of
       Nothing-> return ()
       Just file -> do
         setCurrentFile file ref
         writeFile file =<< getText =<< getSourceBuffer ref

-- |removes @*@ from window title if existing and updates state
deleteStar :: AstAction ()
deleteStar ref = do
  w <- getWindow ref
  (t :: String) <- get w windowTitle
  bufferChanged <- getChanged ref
  when bufferChanged $ set w [windowTitle := tail t]
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
  s :: String <- textBufferGetText buffer start end True
  clipboardSetText clipBoard s

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
actionDeleteSource ref = void $ do
  buffer <- getSourceBuffer ref
  textBufferDeleteSelection buffer False False

-- |launches a dialog which displays the text position associated to
-- last clicked tree node.
actionJumpToTextLoc :: AstAction ()
actionJumpToTextLoc ref = do
  maybeLang <- getLanguage ref
  case maybeLang of
    Nothing -> return ()
    Just lang -> do
      astOrError <- actionGetAst lang ref
      case astOrError of
        Left _    -> return ()
        Right (Ast ast) -> do
          gtkPath <- getPath ref
          unless (null gtkPath) $ do
            let astPath = tail gtkPath
                loc = ast `at` astPath
            case loc of
              Nothing -> return ()
              Just l  -> actionSelectSrcLoc l ref

-- |selects the given source location in gui textview
actionSelectSrcLoc :: SrcSpan -> AstAction ()
actionSelectSrcLoc (SrcSpan (SrcPos bl br)  (SrcPos el er)) ref = do
  textBuffer <- getSourceBuffer ref
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
getCursorPosition ref = do
  (startIter,endIter) <- textBufferGetSelectionBounds =<< getSourceBuffer ref
  lineStart <- textIterGetLine startIter
  rowStart <- textIterGetLineOffset startIter
  lineEnd <- textIterGetLine endIter
  rowEnd <- textIterGetLineOffset endIter
  return $ span (lineStart+1) (rowStart+1) (lineEnd+1) (rowEnd+1)

-- |opens tree position associated with current cursor position.
actionJumpToSrcLoc :: AstAction ()
actionJumpToSrcLoc ref = do
  treePath <- actionGetAssociatedPath ref
  case treePath of
    Just p  -> activatePath p ref
    Nothing -> return ()

-- |returns the shortest path in tree which is associated with the
-- current selected source location.
actionGetAssociatedPath :: AstAction (Maybe Path)
actionGetAssociatedPath ref = do
  sele <- getCursorPosition ref
  maybeLang <- getLanguage ref
  case maybeLang of
    Nothing -> return Nothing
    Just lang -> do
      astOrError <- actionGetAst lang ref
      case astOrError of
        Left _    -> return Nothing
        Right ast ->
           return $ smallestSrcLocContainingCursorPos sele ast


-- |select tree path
activatePath :: Path -> AstAction ()
activatePath p ref = do
  view <- getTreeView ref
  treeViewExpandToPath view p
  treeViewExpandRow view p True
  treeViewSetCursor view p Nothing

-- -------------------------------------------------------------------
-- ** other actions
-- -------------------------------------------------------------------

-- | adds '*' to window title if file changed and sets state
actionBufferChanged :: AstAction ()
actionBufferChanged ref = do
  w <- fmap window (getGui ref)
  t <- get w windowTitle
  c <- getChanged ref
  unless c $ set w [windowTitle := '*':t]
  setChanged True ref

-- | destroys window widget
actionQuit :: AstAction ()
actionQuit ref = do
  isChanged <- getChanged ref
  when isChanged $ actionQuitWorker ref
  actionQuitForce ref

-- |ends program with force
actionQuitForce :: AstAction ()
actionQuitForce ref = do
  widgetDestroy =<< fmap window (getGui ref)

actionQuitWorker :: AstAction ()
actionQuitWorker ref = do
  file <- getCurrentFile ref
  dialog <- messageDialogNew Nothing [] MessageQuestion ButtonsYesNo
    ("Save changes to document \""++takeFileName file ++ "\" before closing?")
  containerSetBorderWidth dialog 2
  widgetShowAll dialog
  response <- dialogRun dialog
  case response of
    ResponseYes   -> actionSave ref
    _             -> actionQuitForce ref
  widgetHide dialog


-- | launches open dialog
actionDlgOpen :: AstAction ()
actionDlgOpen = actionMkDialog FileChooserActionOpen onOkay where
  onOkay dia ref = whenJustM (fileChooserGetFilename dia) $ \file ->
    actionLoadHeadless file ref

-- | launches save dialog
actionDlgSave :: AstAction ()
actionDlgSave = actionMkDialog FileChooserActionSave onOkay where
  onOkay dia ref = do
     maybeFile <- fileChooserGetFilename dia
     case maybeFile of
       Nothing-> return ()
       Just file -> do
          g <- getGui ref
          setChanged False ref
          setCurrentFile file ref
          writeFile file =<< getText =<< getSourceBuffer ref
          set (window g) [windowTitle := takeFileName file]

-- |applies current parser to sourcebuffer
actionReparse :: AstAction ()
actionReparse ref =
  whenJustM (getLanguage ref) $ \l -> void $ actionParse l ref

actionGetPath :: AstAction Path
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
  whenJust m action

-- |returns the text in given text buffer
getText :: TextBufferClass c => c -> IO String
getText tb = do
  start <- textBufferGetStartIter tb
  end <- textBufferGetEndIter tb
  textBufferGetText tb start end True

-- |uses the given string to set the title of given window with
-- suffix "-astview". Window titles should only be set by this
-- function, hence it replaces the corresponding gtk function.
windowSetTitleSuffix :: WindowClass w => w -> String -> IO ()
windowSetTitleSuffix win title = set win [windowTitle := title++" - astview" ]

-- |safe function to write files
writeFile :: FilePath -> String -> IO ()
writeFile f str = withFile f WriteMode (\h -> hPutStr h str >> hClose h)
