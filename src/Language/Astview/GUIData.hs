{- contains the GUI data types
 -
 -}
module Language.Astview.GUIData where

-- state
import Data.IORef (IORef)

-- gtk
import Graphics.UI.Gtk  

-- gtksourceview
import Graphics.UI.Gtk.SourceView (SourceBuffer) 

-- astview-utils
import Language.Astview.Parser (Parser)

-- | a simple container type to ease access to GUI components
data GUI = GUI {
    window :: Window              -- ^ main window
  , tv :: TreeView                -- ^ treeview
  , tb :: SourceBuffer            -- ^ sourceview
  , rFile :: IORef String         -- ^ current file
  , rChanged :: IORef Bool        -- ^ true if file changed
  , rParsers:: IORef [Parser]     -- ^ parsers
  , rCurParser :: IORef Parser    -- ^ current parser
  , dlgAbout :: AboutDialog       -- ^ about dialog
  , cbox :: ComboBox
  }

-- | takes a reference to the compound GUI type and performs
--   some action on the GUI. see section module GUIActions
type GUIAction = GUI -> IO ()

-- | pairs a gtk-id with a GUIAction to easily map over all MenuItems
type MenuAction = (String,GUIAction)
