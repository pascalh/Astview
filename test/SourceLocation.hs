{-| This module contains test cases for functions on source locations. -}
module SourceLocation(testSourceLocations) where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Prelude hiding (span)
import Language.Astview.Language
import Control.Exception(Exception,evaluate,try)
import Control.Monad(unless)

testSourceLocations :: TestTree
testSourceLocations =
  testGroup "Source locations" [groupContains,smartConstructors]

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
  ,propDuality
  ]

propReflexivity :: TestTree
propReflexivity = testGroup "Reflexivity" [propLEQ,propGEQ] where

  propLEQ = testProperty "Reflexivity of <=" prop where

    prop :: SrcSpan -> Bool
    prop s = s <= s

  propGEQ = testProperty "Reflexivity of >=" prop where

    prop :: SrcSpan -> Bool
    prop s = s >= s

propDuality:: TestTree
propDuality = testProperty "Duality of < and >" prop where

  prop :: SrcSpan -> SrcSpan -> Bool
  prop a b
    | a < b     = b > a && a /= b
    | a == b    = b == a
    | a > b     = b < a && a /= b
    | otherwise = True

groupInOneline :: TestTree
groupInOneline = testGroup "Everything in one line" $ map (testCase [])

  [ linear 3 1 2 > linear 3 1 2 @?= False
  , linear 4 1 9 > linear 4 3 6 @?= True
  , linear 4 0 6 < linear 4 1 9 @?= False
  , linear 4 2 9 < linear 4 1 7 @?= False
  , linear 4 1 9 > linear 4 1 9 @?= False
  ]


groupSorrounded :: TestTree
groupSorrounded = testGroup "Point sorrounded by span" $ map (testCase [])

  [ span 4 9 7 9 > span 5 18 6 100 @?= True
  , span 4 9 7 9 > span 5 1 6 1 @?= True
  ]


groupSameBegin :: TestTree
groupSameBegin = testGroup "Same begin line" $ map (testCase [])

  [ span 4 9 7 9 > span 4 18 6 100 @?= True
  , span 4 9 6 9 > span 4 18 7 100 @?= False
  ]

groupSameEnd :: TestTree
groupSameEnd = testGroup "Same end line"  $ map (testCase [])

  [ span 4 9 7 9 > span 5 18 7 5 @?= True
  , span 1 9 7 9 > span 4 18 7 10 @?= False
  ]

groupEx :: TestTree
groupEx = testGroup "Extreme cases"

  [ testCase "equal position" $ span 1 9 7 9 > span 1 9 7 9 @?= False
  , testCase "same end"       $ span 1 1 7 9 > span 1 2 7 9 @?= True
  , testCase "same begin"     $ span 1 9 7 9 > span 1 9 7 3 @?= True
  ]

smartConstructors :: TestTree
smartConstructors = testGroup "Smart constructors"
  [ testCase "span works"     $ span 1 2 3 4  @=? SrcSpan (SrcPos 1 2) (SrcPos 3 4)
  , testCase "span throws exception if begin line > end line" $
        assertException (SrcLocException $ spanUnsafe 2 1 1 1) (span 2 1 1 1)
  , testCase "span throws exception if begin line equals end line and begin column > end column" $
        assertException (SrcLocException $ spanUnsafe 1 5 1 3) (span 1 5 1 3)
  , testCase "position works" $ position 3 4  @=? span 3 4 3 4
  , testCase "linear works"   $ linear 1 2 5  @=? span 1 2 1 5
  , testCase "linear throws exception if begin row > end row" $
      assertException (SrcLocException $ spanUnsafe 1 3 1 1) (linear 1 3 1)
  ]

  -- * Util functions

-- |unsafe variant of 'span'
spanUnsafe :: Int -> Int -> Int -> Int -> SrcSpan
spanUnsafe bl bc el ec = SrcSpan (SrcPos bl bc) (SrcPos el ec)

-- |expects the exception @e@ to occur whhen trying to evaluate @t@
assertException :: (Show a,Show e,Exception e,Eq e) => e -> a -> Assertion
assertException e t =
  let failure x = assertFailure $ "Expected exception ["++show e++"] but got "++show x
  in do
    result <- try (evaluate t)
    case result of
      Left exception -> unless (e == exception) $ failure exception
      Right t        -> failure t
