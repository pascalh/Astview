module Language.Astview.Registry where

-- hint
import Language.Haskell.Interpreter hiding ((:=),set)

-- glob
import System.FilePath.Glob (compile,globDir)

-- astview-utils
import Language.Astview.Parser

-- local
import Paths_astview (getDataFileName,getDataDir) -- by cabal


-- | loads the parserRegistration and all modules in the data dir
loadParsers :: IO [Parser]
loadParsers = do
  -- find additional modules in data
  (glob,_) <- globDir [compile "data/**/*.hs"] =<< getDataDir
  let modules = head glob
  -- run Interpreter
  parsers' <- runInterpreter $ interpretParsers modules
  case parsers' of
    Right parser -> return parser
    Left err -> error (show err)

-- | interprets the modules and returns all parsers found.
interpretParsers :: [FilePath] -> Interpreter [Parser]
interpretParsers modules = do 
  loadModules modules
  setTopLevelModules ["Parsers"]
  return =<< interpret "parsers" (as :: [Parser])

