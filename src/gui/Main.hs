module Main where

-- order of imports analogous to cabal build-depends

-- base
import System.Environment(getArgs)

-- gtk
import Graphics.UI.Gtk hiding (get)

-- local
import Language.Astview.Gui.Actions (actionEmptyGUI,actionLoadHeadless)
import Language.Astview.Gui.Types(getGui,window)
import Language.Astview.Gui.Init(setupGUI)


-- --------------------------------------------------------
-- * main ()
-- --------------------------------------------------------

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
