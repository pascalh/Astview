{-# LANGUAGE Rank2Types #-}
module Main where

-- order of imports analogous to cabal build-depends

-- base
import System.Environment(getArgs)

-- gtk
import Graphics.UI.Gtk  -- (import all)

-- hint
import Language.Haskell.Interpreter hiding ((:=),set)

-- astview-utils
import Language.Astview.Parser
import Language.Astview.DataTree

-- local
import Language.Astview.GUI (buildGUI)
import Language.Astview.GUIActions 
  (actionLoadHeadless,actionEmptyGUI)
import Language.Astview.GUIData (window)
import Language.Astview.Registry (loadParsers)

-- --------------------------------------------------------
-- * main ()
-- --------------------------------------------------------

-- | loads ParserRegistration, inits GTK-GUI, checks for a 
-- CLI-argument (one file to parse) and finally starts the GTK-GUI
main :: IO ()
main = do
  parsers <- loadParsers
  gui <- buildGUI parsers

  -- startup
  args <- getArgs
  case length args of
    1 -> actionLoadHeadless (head args) gui
    0 -> actionEmptyGUI gui
    _ -> error "Zero or one parameter expected"
   
   -- show UI
  widgetShowAll (window gui)
  mainGUI

