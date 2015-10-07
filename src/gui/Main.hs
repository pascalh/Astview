module Main where

import System.Environment(getArgs)
import Graphics.UI.Gtk hiding (get)

import Language.Astview.Gui.Actions (actionEmptyGUI,actionLoadHeadless)
import Language.Astview.Gui.Types(getGui,window)
import Language.Astview.Gui.Init(setupGUI)

-- | loads LanguageRegistration, inits GTK-GUI, checks for a
-- CLI-argument (one file to parse) and finally starts the GTK-GUI
main :: IO ()
main = do
  ref <- setupGUI

  args <- getArgs
  case args of
    []  -> actionEmptyGUI ref
    [a] -> actionLoadHeadless a ref
    _   -> error "Zero or one argument expected"

  widgetShowAll =<< fmap window (getGui ref)
  mainGUI
