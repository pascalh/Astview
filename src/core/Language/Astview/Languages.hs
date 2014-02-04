{-
This File exports the list of known parsers for astview.
You can extend the list with your own parsers 

-}

module Language.Astview.Languages(languages) where

-- -- local imports
import Language.Astview.Language (Language)
import Haskell  
import Python

-- |all languages, whose ast can be viewed in astview 
languages :: [Language]
languages = [haskellexts,python]
