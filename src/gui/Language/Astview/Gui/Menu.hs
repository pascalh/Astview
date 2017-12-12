{- | This module creates the menu bar and binds Actions
to the respective MenuItems.
-}
module Language.Astview.Gui.Menu (initMenu,connect,builderGetObjectStr) where

import           Language.Astview.Gui.Actions
import           Language.Astview.Gui.Types
import           Language.Astview.Language
import           Language.Astview.Languages   (languages)

import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader
import           Data.List                    (intercalate)
import           Data.Monoid                  ((<>))
import           Graphics.UI.Gtk              hiding (Language)
import           Paths_astview                (getDataFileName)
import           System.FilePath              ((</>))
import           System.Glib.UTFString        (stringToGlib)

-- |sets up the menu and binds menu items to logic
initMenu :: Builder -> AstAction ()
initMenu builder = do
  uiManager <- liftIO uiManagerNew
  menuDeclFile <- liftIO $ getDataFileName ("data" </> "menu.xml")
  liftIO $ uiManagerAddUiFromFile uiManager menuDeclFile
  uiManagerBuildLanguagesMenu uiManager

  actionGroup <- liftIO $ actionGroupNew ("ActionGroup" :: String)
  initMenuFile actionGroup
  initMenuEdit actionGroup
  initMenuNavigate actionGroup
  initMenuLanguages actionGroup
  initMenuHelp actionGroup builder

  liftIO $ do
    uiManagerInsertActionGroup uiManager actionGroup 0
    maybeMenubar <- uiManagerGetWidget uiManager ("/ui/menubar" :: String)
    let menubar = case maybeMenubar of
                   Nothing -> error $ "Could not parse menu bar declaration from "
                                      ++ show menuDeclFile
                   Just m  -> m
    vboxMain <- builderGetObjectStr builder castToBox "vboxMain"
    vboxMain `set` [ containerChild := menubar ]
    boxReorderChild vboxMain menubar 0

-- |creates a menu item for every element of 'knownLanguages' in menu "Languages".
--
-- We do this dynamically, because the menu.xml should not contain any static
-- information about specific languages. This offers simple addition of
-- new languages by just adding it to the list of languages without even
-- touching any gui component.
uiManagerBuildLanguagesMenu :: UIManager -> AstAction ()
uiManagerBuildLanguagesMenu uiManager = do
  langs <- getKnownLanguages
  liftIO $ do
    forM_ langs $ \lang -> do
      mergeId <- uiManagerNewMergeId uiManager
      let ident = "actionLanguage"++name lang
      uiManagerAddUi uiManager mergeId "/ui/menubar/Languages/LangsSep"
                 (ident :: String)
                 (Just ident)
                 [UiManagerMenuitem]
                 False

-- |the association between the gui functions from 'Actions'
-- and the gtk identifier from xml file.
menuActions :: [(String,AstAction ())]
menuActions = menuFile ++ menuEdit ++ menuNavigate where
  menuFile =
    [("actionNew",actionEmptyGUI)
    ,("actionSaveAs",actionSaveAs)
    ,("actionOpen",actionDlgOpen)
    ,("actionSave",actionSave)
    ,("actionQuit",actionQuit)
    ]
  menuEdit =
    [("actionCut",actionCutSource)
    ,("actionCopy",actionCopySource)
    ,("actionPaste",actionPasteSource)
    ,("actionDelete",actionDeleteSource)
    ,("actionReparse",actionReparse)
    ,("actionFlatten",actionReparse)
    ]
  menuNavigate =
    [("actionTreeLoc",actionJumpToSrcLoc)
    ,("actionTextLoc",actionJumpToTextLoc)
    ]

-- |associate the menu action with the respective
-- gui function from module Actions
connect :: Action -> AstAction (ConnectId Action)
connect action = do
  st <- ask
  liftIO $ do
    name <- actionGetName action
    case lookup name menuActions of
      Nothing -> error $ "No action associated with "++ show name
      Just f  -> action `on` actionActivated $ runReaderT f st


-- * the menu File

initMenuFile :: ActionGroup -> AstAction ()
initMenuFile actionGroup = do
  actions <- liftIO $ do
    actionFile <- actionNewStr "actionMenuFile" "File" Nothing Nothing

    actionNew <- actionNewStr "actionNew" "New" Nothing (Just stockNew)
    actionOpen <- actionNewStr "actionOpen" "Open"  Nothing (Just stockOpen)
    actionSave <- actionNewStr "actionSave" "Save"    Nothing (Just stockSave)
    actionSaveAs <- actionNewStr "actionSaveAs" "Save As" Nothing (Just stockSaveAs)
    actionQuit <- actionNewStr "actionQuit" "Quit"    Nothing (Just stockQuit)
    actionGroupAddAction actionGroup actionFile
    return [actionNew,actionOpen,actionSave,actionSaveAs,actionQuit]

  forM_  actions $ \action -> do
    connect action
    liftIO $ addAction actionGroup action Nothing


-- * the menu Edit

initMenuEdit :: ActionGroup -> AstAction ()
initMenuEdit actionGroup = do

  actionReparse <- liftIO $ actionNewStr "actionReparse" "Reparse"    Nothing (Just stockRefresh)
  actions <- liftIO $ do
    actionEdit <- actionNewStr "actionMenuEdit" "Edit" Nothing Nothing

    actionCut <- actionNewStr "actionCut" "Cut" Nothing (Just stockCut)
    actionCopy <- actionNewStr "actionCopy" "Copy"  Nothing (Just stockCopy)
    actionPaste <- actionNewStr "actionPaste" "Paste"    Nothing (Just stockPaste)
    actionDelete <- actionNewStr "actionDelete" "Delete" Nothing (Just stockRemove)
    actionGroupAddAction actionGroup actionEdit
    return [actionCut,actionCopy,actionPaste,actionDelete,actionReparse]

  forM_ actions $ \action -> do
    connect action
    liftIO $ addAction actionGroup action Nothing

  liftIO $ actionSetAccelPath actionReparse ("<Control>p" :: String)
  initMenuItemFlatten actionGroup

-- |bind the check menu for flattening lists to the boolean value in the state.
initMenuItemFlatten :: ActionGroup -> AstAction ()
initMenuItemFlatten actionGroup = do
  isFlat <- getFlattenLists
  st <- ask
  let actionToggleFlatten = ToggleActionEntry "actionFlatten"
                                              "Flatten lists in tree?"
                                               Nothing Nothing Nothing (runReaderT f st) isFlat

      f :: AstAction ()
      f = do
        isFlat <- getFlattenLists
        setFlattenLists (not isFlat)
        actionReparse

  liftIO $ actionGroupAddToggleActions actionGroup [actionToggleFlatten]

-- * the menu Navigate

initMenuNavigate :: ActionGroup -> AstAction ()
initMenuNavigate actionGroup = do
  actions <- liftIO $ do
    actionNavigate <- actionNewStr "actionMenuNavigate" "Navigate" Nothing Nothing

    actionTreeLoc <- actionNewStr "actionTreeLoc" ">>>" Nothing Nothing
    actionTextLoc <- actionNewStr "actionTextLoc" "<<<"  Nothing Nothing
    actionGroupAddAction actionGroup actionNavigate
    return [actionTreeLoc,actionTextLoc]

  forM_ actions $ \action -> do
    connect action
    liftIO $ addAction actionGroup action Nothing


-- * the menu Languages

-- |sets up the menu @Languages@ and binds actions to the menu items.
initMenuLanguages :: ActionGroup -> AstAction ()
initMenuLanguages actionGroup = do
  langs <- getKnownLanguages
  st <- ask
  liftIO $ do
    actionLangs <- actionNewStr "actionMenuLanguages" "Languages" Nothing Nothing
    actionGroupAddAction actionGroup actionLangs
    let auto = RadioActionEntry
                 "actionLanguageAuto"
                 "Automatically select languages"
                 Nothing Nothing Nothing 0
        raes = auto:languagesToRadioActionEntry langs
    actionGroupAddRadioActions actionGroup raes 0 (\a -> runReaderT (onRadioChange a) st)

-- |creates a 'RadioActionEntry' for every language
languagesToRadioActionEntry :: [Language] -> [RadioActionEntry]
languagesToRadioActionEntry languages = zipWith mkRadioActionEntry languages [1..] where
  mkRadioActionEntry :: Language -> Int -> RadioActionEntry
  mkRadioActionEntry lang i =
    RadioActionEntry ("actionLanguage" <> stringToGlib (name lang))
                     (stringToGlib $ makeLanguageLabel lang)
                     Nothing Nothing Nothing i

-- |bind functionality to RadioAction
onRadioChange :: RadioAction -> AstAction ()
onRadioChange action = do
  i <- liftIO $ radioActionGetCurrentValue action
  if i == 0
  then
    setActiveLanguage Nothing
  else
    let lang = languages !! (i-1) in
    setActiveLanguage (Just lang)
  actionReparse

-- |produces a string containing the languages' name and
-- the associated file extensions
makeLanguageLabel :: Language -> String
makeLanguageLabel language =
  name language ++
  "   [" ++
  intercalate "," (map (\l -> "*"++l) $ exts language)++
  "]"

-- * the language Help

initMenuHelp :: ActionGroup -> Builder -> AstAction ()
initMenuHelp actionGroup _ = liftIO $ do
  actionHelp <- actionNewStr "actionMenuHelp" "Help" Nothing Nothing
  actionAbout <- actionNewStr "actionAbout" "About"  Nothing (Just stockAbout)
  actionGroupAddAction actionGroup actionHelp
  addAction actionGroup actionAbout Nothing
  license <- getLicense
  actionAbout `on` actionActivated $ do
    dialog <- aboutDialogNew
    set dialog [ aboutDialogWebsite := ("https://github.com/pascalh/Astview"::String)
               , aboutDialogProgramName := ("Astview"::String)
               , aboutDialogComments := aboutComment
               , aboutDialogWrapLicense := True
               , aboutDialogLicense := Just license
               , aboutDialogAuthors := authors
               ]
    widgetShow dialog
    dialog `on` response $ \_ -> widgetHide dialog
    return ()
  return ()

aboutComment :: String
aboutComment =
  "astview: View abstract syntax trees for your custom languages and "++
  "parsers in a graphical (GTK+) application."

getLicense :: IO String
getLicense = readFile =<< getDataFileName "LICENSE"

authors :: [String]
authors =
  [ "Pascal Hof &lt;pascal.hof@tu-dortmund.de&gt; (2009-)"
  , "Sebastian Menge &lt;sebastian.menge@tu-dortmund.de&gt; (2009-2011)"
  ]

-- ** helper functions

actionNewStr :: String -> String -> Maybe String -> Maybe StockId -> IO Action
actionNewStr = actionNew

addAction :: ActionGroup -> Action -> Maybe String -> IO ()
addAction = actionGroupAddActionWithAccel

builderGetObjectStr :: GObjectClass cls => Builder -> (GObject -> cls) -> String -> IO cls
builderGetObjectStr = builderGetObject
