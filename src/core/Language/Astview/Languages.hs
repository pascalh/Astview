{- |
This module exports the list of languages known to astview.
You can extend it with your own languages. See
documentation of 'Language' for details.

-}

module Language.Astview.Languages(languages) where

import Language.Astview.Language (Language)
import Language.Astview.Languages.Haskell (haskellExts)
import Language.Astview.Languages.Python (python)
import Language.Astview.Languages.HaskellCore (haskellCore)

-- |all languages, whose abstract syntax trees can be viewed in astview.
languages :: [Language]
languages = [haskellExts,python,haskellCore]
