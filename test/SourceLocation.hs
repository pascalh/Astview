{-| This module contains test cases for functions on source locations. -}
module SourceLocation(testSourceLocations) where
import Test.HUnit hiding (Test)

import Test.Tasty
import Test.Tasty.QuickCheck 
import Test.Tasty.HUnit

import Data.Tree

import Language.Astview.Language
import Language.Astview.Pathlist
import Language.Astview.SmallestSrcLocContainingCursor 
  (smallestSrcLocContainingCursorPos) 
import Language.Astview.DataTree(annotateWithPaths)

testSourceLocations :: TestTree
testSourceLocations = 
  testGroup "Source locations" [groupContains,groupSelect]

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


-- * select

groupSelect :: TestTree
groupSelect = 
  testGroup "selecting the greatest sorrounding source location" 
            [t1,t2,t3,t4,t5,t6]

-- |a shorter name
select :: CursorSelection -> Ast -> PathList
select = smallestSrcLocContainingCursorPos 

mkTree :: String -> SrcLocation -> [Tree AstNode] -> Tree AstNode
mkTree l s cs = annotateWithPaths $ Node (AstNode l (Just s) [] Identificator) cs

t1 :: TestTree
t1 = testCase "return first occourence" $ 
       toList (select (CursorSelection 1 2 1 7) (Ast ast)) @?= [(SrcSpan 1 2 1 7,[0])]  where
          ast = mkTree "a" (SrcSpan 1 2 1 7) []

t2 :: TestTree
t2 = testCase "return immediate successor" $ 
       let r = SrcSpan 1 2 3 9
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c]
           c =  mkTree "b" r []
       in
       select (CursorSelection 1 3 3 6) (Ast ast) 
       @?= 
       singleton (r,[0,0]) 

t3 :: TestTree
t3 = testCase "return root if successor does not match" $ 
       let r = SrcSpan 1 1 19 7 
           ast = mkTree "a" r [c]  
           c =  mkTree "b" (SrcSpan 10 2 17 9 ) []
       in
       select (CursorSelection 1 2 3 9) (Ast ast) 
       @?= 
       singleton (r,[0]) 


t4 :: TestTree
t4 = testCase "return leaf in three containing spans" $ 
       let r = SrcSpan 2 1 4 2
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c1]
           c1 =  mkTree "b" (SrcSpan 1 1 5 9) [c2]
           c2 =  mkTree "b"  r []
       in
       select (CursorSelection 2 1 3 1) (Ast ast) 
       @?= 
       singleton (r,[0,0,0]) 

t5 :: TestTree
t5 = testCase "triangle, select the correct child" $ 
       let r = SrcSpan 2 1 4 5
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c1,c2]  
           c1 =  mkTree "b" (SrcSpan 10 1 15 9) []
           c2 =  mkTree "b" r []
       in
       select (CursorSelection 2 1 3 1) (Ast ast) 
       @?= 
       singleton (r,[0,1]) 

t6 :: TestTree
t6 = testCase "triangle, select multiple locations" $ 
       let r = SrcSpan 2 1 4 2
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c1,c2]  
           c1 =  mkTree "b" (SrcSpan 10 1 15 9) []
           c2 =  mkTree "b" r [c3]
           c3 =  mkTree "b" r []
       in
       select (CursorSelection 2 1 3 1) (Ast ast)
       @?= 
       ins (r,[0,1]) (singleton (r,[0,1,0]))
