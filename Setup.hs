#!/usr/bin/runhaskell

import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags (..))
import Distribution.PackageDescription (emptyHookedBuildInfo,HookedBuildInfo(..))
import Language.Haskell.HsColour (hscolour,Output(CSS))
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks { preConf = myPreConf }

myPreConf :: Args -> ConfigFlags -> IO HookedBuildInfo
myPreConf args cf = do
  makedocs 
  return emptyHookedBuildInfo

makedocs :: IO ()
makedocs = do
  putStr "Generating custom html documentation... "
  tmpl <- readFile "data/astview-tmpl.html"
  let ls = lines tmpl
  ls' <- mapM replaceOrEcho ls
  putStrLn " done."
  let ls'' = concat ls'
  writeFile "data/astview.html" (unlines ls'')
  return ()


-- echoes the current line, or, if mymatch succeeds:
-- replaces the line with colourized haskell code.
replaceOrEcho :: String -> IO [String]
replaceOrEcho s = if not $ mymatch s 
  then return [s]
  else do
    putStr $ (exex s)++" "
    ex <- readFile ("data/"++(exex s)++".hs")
    let ls = lines $ hscolour CSS defaultColourPrefs False True (exex s) False ex 
    return (["<!-- Example "++(exex s)++" follows: -->"]
           ++ ls
           ++ ["<!-- Example "++(exex s)++" done. -->"])


-- recognizes Template marker of the form "%%asdf%%"
mymatch :: String -> Bool
mymatch s = take 2 s == "%%" && take 2 (reverse s) == "%%"

--extracts the filename from the marker
exex :: String -> String
exex s = let remainder = (drop 2 s) in reverse (drop 2 (reverse remainder) )
