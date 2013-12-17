module Main where
import Test.Tasty (defaultMain)

import SourceLocation(testSourceLocations)

main :: IO ()
main = defaultMain testSourceLocations
