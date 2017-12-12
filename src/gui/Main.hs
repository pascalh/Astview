module Main where

import           Control.Monad.Reader
import           Graphics.UI.Gtk              hiding (get)
import           Paths_astview                (getDataFileName)
import           System.Environment           (getArgs)
import           System.FilePath              ((</>))

import           Language.Astview.Gui.Actions (actionEmptyGUI,
                                               actionLoadHeadless)
import           Language.Astview.Gui.Init    (hooks, setupGui)
import           Language.Astview.Gui.Menu    (initMenu)
import           Language.Astview.Gui.Types



-- | loads LanguageRegistration, inits GTK-GUI, checks for a
-- CLI-argument (one file to parse) and finally starts the GTK-GUI
main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName ("data" </> "astview.xml")
  ioref <- setupGui builder

  args <- getArgs
  flip runReaderT ioref $ do

    initMenu builder
    hooks

    case args of
      []     -> actionEmptyGUI
      [file] -> actionLoadHeadless file
      _      -> error "Zero or one argument expected"

    w <- getWindow
    liftIO $ do
      widgetShowAll w
      mainGUI
