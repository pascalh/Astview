{-| This module contains test cases for functions on source locations. -}
module SourceLocation(testSourceLocations) where
import Test.HUnit hiding (Test)

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit

import Language.Astview.Language(SrcLocation(SrcSpan))

testSourceLocations :: TestTree
testSourceLocations = 
  testGroup "Source locations" [groupContains]

groupContains :: TestTree
groupContains = 
  testGroup "ord instance for source locations implements contains" 
            [groupInOneline
            ,groupSorrounded
            ,groupSameBegin
            ,groupSameEnd
            ,groupEx
            ,groupAlgebraicProperties
            ]

groupAlgebraicProperties :: TestTree
groupAlgebraicProperties = testGroup "algebraic properties" 
  [propReflexivity 
  ]

propReflexivity :: TestTree
propReflexivity = testGroup "Reflexivity" [propLEQ,propGEQ] where

  propLEQ = testProperty "Reflexivity of <=" p where
    p :: SrcLocation -> Bool
    p s = s <= s 

  propGEQ = testProperty "Reflexivity of >=" p where
    p :: SrcLocation -> Bool
    p s = s >= s 

groupInOneline :: TestTree
groupInOneline = testGroup "Everything in one line" $ map (testCase []) 

  [ SrcSpan 3 1 3 2 > SrcSpan 3 1 3 2 @?= False 
  , SrcSpan 4 1 4 9 > SrcSpan 4 3 4 6 @?= True
  , SrcSpan 4 0 4 6 < SrcSpan 4 1 4 9 @?= False 
  , SrcSpan 4 2 4 16 < SrcSpan 4 1 4 9 @?= False 
  , SrcSpan 4 1 4 9 > SrcSpan 4 1 4 9 @?= False 
  ]


groupSorrounded :: TestTree
groupSorrounded = testGroup "Point sorrounded by span" $ map (testCase []) 

  [ SrcSpan 4 9 7 9 > SrcSpan 5 18 6 100 @?= True 
  , SrcSpan 4 9 7 9 > SrcSpan 5 1 6 1 @?= True 
  ]


groupSameBegin :: TestTree
groupSameBegin = testGroup "Same begin line" $ map (testCase []) 

  [ SrcSpan 4 9 7 9 > SrcSpan 4 18 6 100 @?= True 
  , SrcSpan 4 9 6 9 > SrcSpan 4 18 7 100 @?= False 
  ]

groupSameEnd :: TestTree
groupSameEnd = testGroup "Same end line"  $ map (testCase []) 

  [ SrcSpan 4 9 7 9 > SrcSpan 5 18 7 5 @?= True 
  , SrcSpan 1 9 7 9 > SrcSpan 4 18 7 10 @?= False 
  ]

groupEx :: TestTree
groupEx = testGroup "Extreme cases" 

  [ testCase "equal position" $ SrcSpan 1 9 7 9 > SrcSpan 1 9 7 9 @?= False 
  , testCase "same end"       $ SrcSpan 1 1 7 9 > SrcSpan 1 2 7 9 @?= True 
  , testCase "same begin"     $ SrcSpan 1 9 7 9 > SrcSpan 1 9 7 3 @?= True 
  ]
