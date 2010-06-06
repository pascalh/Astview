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
  , config :: Configuration -- ^ current configuration
  , configFile :: FilePath -- ^ path of current configuraton file 
  }

-- |main gui data type, contains gtk components
data GUI = GUI
  { window :: Window -- ^ main window
  , tv :: (TreeView,TreeView) -- ^ treeview
  , sb :: (SourceBuffer,SourceBuffer) -- ^ sourceview
  , tvConf :: TextView -- ^ text view showing the config file
  , dlgAbout :: AboutDialog -- ^ about dialog
  , cbox :: ComboBox -- ^ combobox containing the languages
  }

-- |indicator data type for both areas
data Area = L -- ^ left area 
          | R -- ^ right area

-- |a configuration contains of relatons between nodes
data Configuration = Configuration
  { relations :: [Relation]
  }

-- |data type to specify binary relations between nodes
data Relation = Relation 
  { e1 :: Elem -- ^ first relation element
  , e2 :: Elem -- ^ second relation element
  }

-- |an element of the relation
data Elem = Elem
  { path :: [Direction]   -- ^ path in ast to a node 
  , filepath :: FilePath  -- ^ file containing the ast
  } 

-- |data type to specify paths in trees, a path has the type 
-- > [Direction]
data Direction 
  = D -- ^ go down one level to the leftmost child
  | Ri -- ^ stay at the same level and go to the right

-- * parser of data type configuration

readConfig :: String -> Configuration
readConfig = Configuration . map readRelation . lines

readRelation :: String -> Relation
readRelation s = 
  let e1 = takeWhile (/=' ') s in
  let e2 = drop (1+length e1) s in
  Relation (readElem e1) (readElem e2)

readElem :: String -> Elem
readElem s = 
  let (p,fp) = span (/='@') s in
  Elem (map readDirection p) (tail fp)
  
readDirection :: Char -> Direction
readDirection 'r' = Ri
readDirection 'd' = D
readDirection _ = error "direction r or d expected"  


-- * getter functions

getConfigFile :: IORef AstState -> IO FilePath
getConfigFile = fmap (configFile . state) . readIORef

getTvConf :: IORef AstState -> IO TextView
getTvConf = fmap (tvConf . gui) . readIORef 

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
  f L s@(AstState (State (_,cR) c ls l a co cf) _ _) = 
    s { state = State (file,cR) c ls l a co cf}
  f R s@(AstState (State (cL,_) c ls l a co cf) _ _) = 
    s { state = State (cL,file) c ls l a co cf}

setcArea :: Area -> IORef AstState -> IO ()
setcArea a r = modifyIORef r f where
  f :: AstState -> AstState
  f s@(AstState (State x c ls l _ co cf) _ _) = 
    s { state = State x c ls l a co cf}

setChanged :: Area -> Bool -> IORef AstState -> IO ()
setChanged a b r = modifyIORef r (f a) where
  f :: Area -> AstState -> AstState
  f L s@(AstState (State f (_,c) ls l a co cf) _ _) = 
    s { state = State f (b,c) ls l a co cf}
  f R s@(AstState (State f (c,_) ls l a co cf) _ _) = 
    s { state = State f (c,b) ls l a co cf}

setLanguage :: Language -> IORef AstState -> IO ()
setLanguage l r = modifyIORef r f where
  f :: AstState -> AstState
  f s@(AstState (State f c ls _ a co cf) _ _) = 
    s { state = State f c ls l a co cf}

setConfiguration :: Configuration -> IORef AstState -> IO ()
setConfiguration c r = modifyIORef r f where
  f :: AstState -> AstState
  f s@(AstState (State f cc ls l a _ cf) _ _) = 
    s { state = State f cc ls l a c cf}

setConfigFile :: FilePath -> IORef AstState -> IO ()
setConfigFile fp r = modifyIORef r f where
  f :: AstState -> AstState
  f s@(AstState (State f cc ls l a c _) _ _) = 
    s { state = State f cc ls l a c fp}

-- * misc transformations

addRelation :: Relation -> IORef AstState -> IO ()
addRelation r ref = modifyIORef ref f where
  f :: AstState -> AstState
  f s@(AstState (State f cc ls l a (Configuration rs) fp) _ _) = 
    s { state = State f cc ls l a (Configuration $ rs++[r]) fp}

-- instances

instance Show Relation where
  show (Relation e1 e2) = show e1 ++" "++ show e2

instance Show Elem where
  show (Elem p file) = show p ++ "@" ++ file 

instance Show Direction where
  show D  = "d"
  show Ri = "r"
  
