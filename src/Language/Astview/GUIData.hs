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
  { cFile :: (String,String) -- ^ current file
  , textchanged :: (Bool,Bool) -- ^ true if file changed
  , languages :: [Language] -- ^ known languages
  , cLang :: Language-- ^ current language
  , cArea :: Area -- ^ containing current area
  }

-- |main gui data type, contains gtk components
data GUI = GUI
  { window :: Window -- ^ main window
  , tv :: (TreeView,TreeView) -- ^ treeview
  , sb :: (SourceBuffer,SourceBuffer) -- ^ sourceview
  , dlgAbout :: AboutDialog -- ^ about dialog
  , cbox :: ComboBox -- ^ combobox containing the languages
  }

-- |indicator data type for both areas
data Area = L -- ^ left area 
          | R -- ^ right area

-- * getter functions

getCArea :: IORef AstState -> IO Area
getCArea = fmap (cArea . state) . readIORef

getSourceBuffer :: Area -> IORef AstState -> IO SourceBuffer
getSourceBuffer a r = do
  let sel = case a of 
              L -> fst
              R -> snd
  fmap (sel . sb . gui) $ readIORef r


getcSourceBuffer :: IORef AstState -> IO SourceBuffer
getcSourceBuffer r = do
  area <- getCArea r
  getSourceBuffer area r

getcTreeView :: IORef AstState -> IO TreeView
getcTreeView r = do
  area <- getCArea r
  let sel = case area of 
              L -> fst
              R -> snd
  fmap (sel . tv . gui) $ readIORef r

getTreeViews :: IORef AstState -> IO [TreeView]
getTreeViews r = do
  t1 <- fmap (fst . tv . gui) $ readIORef r
  t2 <- fmap (snd . tv . gui) $ readIORef r
  return [t1,t2] 

getTreeView :: Area -> IORef AstState -> IO TreeView
getTreeView L = fmap (fst . tv . gui) . readIORef 
getTreeView R = fmap (snd . tv . gui) . readIORef 

getSourceBuffers :: IORef AstState -> IO [SourceBuffer]
getSourceBuffers r = do
  s1 <- fmap (fst . sb . gui) $ readIORef r
  s2 <- fmap (snd . sb . gui) $ readIORef r
  return [s1,s2] 

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

getChanged :: Area -> IORef AstState -> IO Bool
getChanged L = fmap (fst . textchanged . state) . readIORef
getChanged R = fmap (snd . textchanged . state) . readIORef

getFile :: Area -> IORef AstState -> IO String
getFile L = fmap (fst . cFile . state) . readIORef
getFile R = fmap (snd . cFile . state) . readIORef

getcFile :: IORef AstState -> IO String
getcFile r = do
  area <- getCArea r
  getFile area r

getcLang = fmap (cLang . state) . readIORef

getWindow = fmap (window . gui) . readIORef

-- * setter functions

setcFile :: Area -> FilePath -> IORef AstState -> IO ()
setcFile a file r = modifyIORef r (f a) where
  f :: Area -> AstState -> AstState
  f L s@(AstState (State (_,cR) c ls l a) _ _) = 
    s { state = State (file,cR) c ls l a}
  f R s@(AstState (State (cL,_) c ls l a) _ _) = 
    s { state = State (cL,file) c ls l a}

setcArea :: Area -> IORef AstState -> IO ()
setcArea a r = modifyIORef r f where
  f :: AstState -> AstState
  f s@(AstState (State x c ls l _) _ _) = 
    s { state = State x c ls l a}

setChanged :: Area -> Bool -> IORef AstState -> IO ()
setChanged a b r = modifyIORef r (f a) where
  f :: Area -> AstState -> AstState
  f L s@(AstState (State f (_,c) ls l a) _ _) = 
    s { state = State f (b,c) ls l a}
  f R s@(AstState (State f (c,_) ls l a) _ _) = 
    s { state = State f (c,b) ls l a}

setLanguage :: Language -> IORef AstState -> IO ()
setLanguage l r = modifyIORef r f where
  f :: AstState -> AstState
  f s@(AstState (State f c ls _ a) _ _) = 
    s { state = State f c ls l a}

