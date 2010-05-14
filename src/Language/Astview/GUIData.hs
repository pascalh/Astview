{-# LANGUAGE ExistentialQuantification #-}
{- contains the GUI data types
 -
 -}
module Language.Astview.GUIData where

import Data.Tree (Tree(..))
import Data.IORef

-- gtksourceview
import Graphics.UI.Gtk hiding (Language,get)
import Graphics.UI.Gtk.SourceView (SourceBuffer) 

-- astview-utils
import Language.Astview.Language (Language)

type AstAction a = IORef AstState -> IO a

-- |union of intern program state and gui
data AstState = AstState
  { state :: State -- ^ intern program state
  , gui :: GUI -- ^ gtk data types
  , options :: Options -- ^ global program options
  }

-- |data type for global options
data Options = Options
  { font :: String -- ^ font name of textbuffer
  , fsize :: Int -- ^ font size of textbuffer
  }

-- |data type for the intern program state
data State =  forall a .  State
  { cFile :: String -- ^ current file
  , textchanged :: Bool -- ^ true if file changed
  , languages :: [Language] -- ^ known languages
  , cLang :: Language-- ^ current language
  , cTree :: a -- ^ current tree
  }

-- |main gui data type, contains gtk components
data GUI = GUI
  { window :: Window -- ^ main window
  , tv :: TreeView -- ^ treeview
  , tb :: SourceBuffer -- ^ sourceview
  , dlgAbout :: AboutDialog -- ^ about dialog
  , cbox :: ComboBox -- ^ combobox containing the languages
  }

-- * getter functions

getAstState :: IORef AstState -> IO AstState
getAstState = readIORef

-- |returns gui data type
getGui :: IORef AstState -> IO GUI
getGui = fmap gui . readIORef 

getState :: IORef AstState -> IO State 
getState = fmap state . readIORef 

getLangs :: IORef AstState -> IO [Language]
getLangs = fmap (languages . state) . readIORef

getcBox :: IORef AstState -> IO ComboBox
getcBox = fmap (cbox . gui) . readIORef

getChanged = fmap (textchanged . state) . readIORef

getcFile = fmap (cFile . state) . readIORef

getcLang = fmap (cLang . state) . readIORef

getWindow = fmap (window . gui) . readIORef
-- * setter functions

setcFile :: FilePath -> IORef AstState -> IO ()
setcFile file r = modifyIORef r f where
  f :: AstState -> AstState
  f a@(AstState (State _ c ls l t) _ _) = 
    a { state = State file c ls l t }

setcTree :: Tree String -> IORef AstState -> IO ()
setcTree t r = modifyIORef r f where
  f :: AstState -> AstState
  f a@(AstState (State f c ls l _) _ _) = 
    a { state = State f c ls l t }

setChanged :: Bool -> IORef AstState -> IO ()
setChanged b r = modifyIORef r f where
  f :: AstState -> AstState
  f a@(AstState (State f _ ls l t) _ _) = 
    a { state = State f b ls l t }

setLanguage :: Language -> IORef AstState -> IO ()
setLanguage l r = modifyIORef r f where
  f :: AstState -> AstState
  f a@(AstState (State f c ls _ t) _ _) = 
    a { state = State f c ls l t }

