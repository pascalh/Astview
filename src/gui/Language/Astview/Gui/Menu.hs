{- | This module creates the menu bar and binds Actions
to the respective MenuItems.
-}
module Language.Astview.Gui.Menu (initMenu,connect,builderGetObjectStr) where

import Language.Astview.Gui.Types
import Language.Astview.Gui.Actions
import Language.Astview.Languages(languages)
import Language.Astview.Language

import Graphics.UI.Gtk hiding (Language)
import Paths_astview (getDataFileName)
import System.FilePath ((</>))
import Data.List(intercalate)
import Data.Monoid ((<>))
import System.Glib.UTFString (stringToGlib)

-- |sets up the menu and binds menu items to logic
initMenu :: Builder -> AstAction ()
initMenu builder ref = do
  actionGroup <- actionGroupNew ("ActionGroup" :: String)
  initMenuFile actionGroup ref
  initMenuEdit actionGroup ref
  initMenuNavigate actionGroup ref
  initMenuLanguages actionGroup ref
  initMenuHelp actionGroup builder ref

  uiManager <- uiManagerNew
  menuDeclFile <- getDataFileName ("data" </> "menu.xml")
  uiManagerAddUiFromFile uiManager menuDeclFile

  uiManagerInsertActionGroup uiManager actionGroup 0
  maybeMenubar <- uiManagerGetWidget uiManager ("/ui/menubar" :: String)
  let menubar = case maybeMenubar of
                 Nothing -> error $ "Could not parse menu bar declaration from "
                                    ++ show menuDeclFile
                 Just m  -> m
  vboxMain <- builderGetObjectStr builder castToVBox "vboxMain"
  boxPackStart vboxMain menubar PackNatural 0

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
connect action ref = do
  name <- actionGetName action
  case lookup name menuActions of
    Nothing -> error $ "No action associated with "++ show name
    Just f  -> action `on` actionActivated $ f ref


-- * the menu File

initMenuFile :: ActionGroup -> AstAction ()
initMenuFile actionGroup ref = do
  actionFile <- actionNewStr "actionMenuFile" "File" Nothing Nothing

  actionNew <- actionNewStr "actionNew" "New" Nothing (Just stockNew)
  actionOpen <- actionNewStr "actionOpen" "Open"  Nothing (Just stockOpen)
  actionSave <- actionNewStr "actionSave" "Save"    Nothing (Just stockSave)
  actionSaveAs <- actionNewStr "actionSaveAs" "Save As" Nothing (Just stockSaveAs)
  actionQuit <- actionNewStr "actionQuit" "Quit"    Nothing (Just stockQuit)
  actionGroupAddAction actionGroup actionFile
  mapM_ (\action -> do {action `connect` ref ; addAction actionGroup action Nothing})
        [actionNew,actionOpen,actionSave,actionSaveAs,actionQuit]

-- * the menu Edit

initMenuEdit :: ActionGroup -> AstAction ()
initMenuEdit actionGroup ref = do
  actionEdit <- actionNewStr "actionMenuEdit" "Edit" Nothing Nothing

  actionCut <- actionNewStr "actionCut" "Cut" Nothing (Just stockCut)
  actionCopy <- actionNewStr "actionCopy" "Copy"  Nothing (Just stockCopy)
  actionPaste <- actionNewStr "actionPaste" "Paste"    Nothing (Just stockPaste)
  actionDelete <- actionNewStr "actionDelete" "Delete" Nothing (Just stockRemove)
  actionReparse <- actionNewStr "actionReparse" "Reparse"    Nothing (Just stockRefresh)
  actionGroupAddAction actionGroup actionEdit
  mapM_ (\action -> do {action `connect` ref ; addAction actionGroup action Nothing})
        [actionCut,actionCopy,actionPaste,actionDelete,actionReparse]

  actionSetAccelPath actionReparse ("<Control>p" :: String)
  initMenuItemFlatten actionGroup ref

-- |bind the check menu for flattening lists to the boolean value in the state.
initMenuItemFlatten :: ActionGroup -> AstAction ()
initMenuItemFlatten actionGroup ref = do
  isFlat <- getFlattenLists ref
  let actionToggleFlatten = ToggleActionEntry "actionFlatten"
                                              "Flatten lists in tree?"
                                               Nothing Nothing Nothing f isFlat
      f = do
        isFlat <- getFlattenLists ref
        setFlattenLists (not isFlat) ref
        actionReparse ref
  actionGroupAddToggleActions actionGroup [actionToggleFlatten]

-- * the menu Navigate

initMenuNavigate :: ActionGroup -> AstAction ()
initMenuNavigate actionGroup ref = do
  actionNavigate <- actionNewStr "actionMenuNavigate" "Navigate" Nothing Nothing

  actionTreeLoc <- actionNewStr "actionTreeLoc" ">>>" Nothing Nothing
  actionTextLoc <- actionNewStr "actionTextLoc" "<<<"  Nothing Nothing
  actionGroupAddAction actionGroup actionNavigate
  mapM_ (\action -> do {action `connect` ref ; addAction actionGroup action Nothing})
        [actionTreeLoc,actionTextLoc]

-- * the menu Languages

-- |sets up the menu @Languages@ and binds actions to the menu items.
initMenuLanguages :: ActionGroup -> AstAction ()
initMenuLanguages actionGroup ref = do
  actionLangs <- actionNewStr "actionMenuLanguages" "Languages" Nothing Nothing
  actionGroupAddAction actionGroup actionLangs
  langs <- getKnownLanguages ref
  let auto = RadioActionEntry
               "actionLanguageAuto"
               "Automatically select languages"
               Nothing Nothing Nothing 0
      raes = auto:languagesToRadioActionEntry langs
  actionGroupAddRadioActions actionGroup raes 0 (`onRadioChange` ref)

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
onRadioChange action ref = do
  i <- radioActionGetCurrentValue action
  if i == 0
  then
    setActiveLanguage Nothing ref
  else
    let lang = languages !! (i-1) in
    setActiveLanguage (Just lang) ref
  actionReparse ref

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
initMenuHelp actionGroup builder _ = do
  actionHelp <- actionNewStr "actionMenuHelp" "Help" Nothing Nothing
  actionAbout <- actionNewStr "actionAbout" "About"  Nothing (Just stockAbout)
  actionGroupAddAction actionGroup actionHelp
  addAction actionGroup actionAbout Nothing
  actionAbout `on` actionActivated $ do
    dialog <- builderGetObjectStr builder castToAboutDialog "dlgAbout"
    widgetShow dialog
    dialog `on` response  $ \ _ -> widgetHide dialog
    return ()
  return ()

-- ** helper functions

actionNewStr :: String -> String -> Maybe String -> Maybe StockId -> IO Action
actionNewStr = actionNew

addAction :: ActionGroup -> Action -> Maybe String -> IO ()
addAction = actionGroupAddActionWithAccel

builderGetObjectStr :: GObjectClass cls => Builder -> (GObject -> cls) -> String -> IO cls
builderGetObjectStr = builderGetObject
