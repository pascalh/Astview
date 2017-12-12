{- contains the main gui functions
 -}

module Language.Astview.Gui.Actions where
import           Language.Astview.DataTree                       (flatten)
import           Language.Astview.Gui.GtkActions
import           Language.Astview.Gui.Types
import           Language.Astview.Language
import           Language.Astview.SmallestSrcLocContainingCursor (smallestSrcLocContainingCursorPos)

import           Control.Monad                                   (unless, void,
                                                                  when)
import           Control.Monad.IO.Class                          (liftIO)
import qualified Data.ByteString.Char8                           as BS (hGetContents,
                                                                        unpack)
import           Data.List                                       (find)
import           Data.Tree                                       (Tree (Node))
import           Prelude                                         hiding
                                                                  (writeFile)
import           System.FilePath                                 (takeExtension,
                                                                  takeFileName)
import           System.IO                                       (IOMode (..),
                                                                  withFile)

-- | resets the GUI,
actionEmptyGUI :: AstAction ()
actionEmptyGUI = do
  clearTreeView
  sourceViewSetText ""
  winSetTitle unsavedDoc

-- | updates the sourceview with a given file and parses the file
actionLoadHeadless :: FilePath -> AstAction ()
actionLoadHeadless file = do
  setCurrentFile file
  winSetTitle (takeFileName file)
  sourceViewSetText =<< liftIO (withFile file ReadMode (fmap BS.unpack . BS.hGetContents))
  deleteStar
  setChanged False
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
  clearTreeView
  setupSyntaxHighlighting l
  tree <- buildTree <$> actionGetAst l
  treeviewSetTree tree

-- |constructs the tree which will be presented by our gtk-treeview
buildTree :: Either Error Ast  -> Tree String
buildTree (Left Err)                 = Node "Parse error" []
buildTree (Left (ErrMessage m))      = Node m []
buildTree (Left (ErrLocation pos m)) = Node ("Parse error at:"++show pos++": "++m) []
buildTree (Right t)                  = label <$> ast t

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
      setChanged False


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

at :: Tree AstNode -> Path -> Maybe SrcSpan
at (Node n _ )  []     = srcspan n
at (Node _ cs) (i:is)  = get i cs >>= \tree -> tree `at` is where

  get :: Int -> [a] -> Maybe a
  get _ [] = Nothing
  get n (x:xs)
    | n <  0    = Nothing
    | n >  0    = get (n-1) xs
    | otherwise = Just x


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


-- -------------------------------------------------------------------
-- ** other actions
-- -------------------------------------------------------------------

-- | destroys window widget
actionQuit :: AstAction ()
actionQuit = do
  isChanged <- getChanged
  when isChanged $ actionQuitWorker actionSave actionQuitForce
  actionQuitForce

-- |applies current parser to sourcebuffer
actionReparse :: AstAction ()
actionReparse =
  whenJustM getLanguage (void . actionParse)
