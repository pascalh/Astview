{-
This File exports the list of known parsers for astview.
You can extend the list with your own parsers 

-}

module Language.Astview.Languages where

-- -- local imports
import Language.Astview.Language (Language)
import Haskell  
import Python

-- |  astview
knownLanguages :: [Language]
knownLanguages = [haskellexts,python]
