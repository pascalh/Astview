module Main where
import Test.Tasty (defaultMain,testGroup)

import SourceLocation(testSourceLocations)
import SmallestSrcLocContainingCursor (testSelect) 

main :: IO ()
main = defaultMain $ testGroup "Tests" [testSourceLocations,testSelect]
