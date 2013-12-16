module Main where
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit(hUnitTestToTests)

import SourceLocation(testSourceLocations)

main :: IO ()
main = defaultMain $ concatMap hUnitTestToTests [testSourceLocations]
