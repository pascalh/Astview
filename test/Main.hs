module Main where
import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit 

import Data.Tree

import Language.Astview.GUIData
import Language.Astview.SourceLocation
import Language.Astview.Language

main :: IO ()
main = defaultMain tests
 
tests =
  [ groupContains
  , groupSelect
  ]

-- * contains

groupContains = 
  testGroup "Contains" 
            [groupInOneline
            ,groupSorrounded
            ,groupSameBegin
            ,groupSameEnd
            ,groupEx
            ]

groupInOneline = testGroup "Everything in one line"  
                           [samePos
                           ,onelinePos
                           ,onelineNeg1
                           ,onelineNeg2
                           ,onelineNeg3
                           ]

samePos = 
  testCase "samePos" $(SrcSpan 3 1 3 2) > (SrcSpan 3 1 3 2) @?= False 

onelinePos = 
  testCase "onelinePos" $(SrcSpan 4 1 4 9) > (SrcSpan 4 3 4 6) @?= True

onelineNeg1 = 
  testCase "neg1" $  (SrcSpan 4 0 4 6) < (SrcSpan 4 1 4 9)@?= False 

onelineNeg2 = 
  testCase "neg2" $  (SrcSpan 4 2 4 16) < (SrcSpan 4 1 4 9)@?= False 

onelineNeg3 = 
  testCase "neg3" $ (SrcSpan 4 1 4 9) > (SrcSpan 4 1 4 9) @?= False 


groupSorrounded = testGroup "Point sorrounded by span" [sorroundedPos,sorroundedPos1]
sorroundedPos = 
  testCase [] $ (SrcSpan 4 9 7 9) > (SrcSpan 5 18 6 100) @?= True 

sorroundedPos1 = 
  testCase [] $ (SrcSpan 4 9 7 9) > (SrcSpan 5 1 6 1) @?= True 

groupSameBegin = testGroup "Same begin line" [beginPos,beginNeg]
beginPos = 
  testCase "Pos" $ (SrcSpan 4 9 7 9) > (SrcSpan 4 18 6 100) @?= True 

beginNeg = 
  testCase "Begin" $ (SrcSpan 4 9 6 9) > (SrcSpan 4 18 7 100) @?= False 


groupSameEnd = testGroup "Same end line" [endPos,endNeg]
endPos= 
  testCase "Pos" $ (SrcSpan 4 9 7 9) > (SrcSpan 5 18 7 5) @?= True 

endNeg= 
  testCase "End too long" $ (SrcSpan 1 9 7 9) > (SrcSpan 4 18 7 10) @?= False 


groupEx = testGroup "Extreme cases" [equalPos,sameBegin,sameEnd]

equalPos = 
  testCase "Equal srclocs" $ (SrcSpan 1 9 7 9) > (SrcSpan 1 9 7 9) @?= False 

sameEnd = 
  testCase "Same end" $ (SrcSpan 1 1 7 9) > (SrcSpan 1 2 7 9) @?= True 

sameBegin = 
  testCase "Same begin" $ (SrcSpan 1 9 7 9) > (SrcSpan 1 9 7 3) @?= True 


-- * select
groupSelect = testGroup "Select" [t1,t2,t3,t4,t5,t6]

t1 = testCase "return first occourence" $ 
       (toList $ select (CursorSelection 1 2 1 7) (Ast ast)) @?= [(SrcSpan 1 2 1 7,[0])]  where
          ast = (Node (AstNode "a" (Just (SrcSpan 1 2 1 7))) [])
       
t2 = testCase "return immediate successor" $ 
       let r = SrcSpan 1 2 3 9
           ast = (Node (AstNode "a" (Just (SrcSpan 1 1 16 3))) [c])  
           c =  Node (AstNode "b" (Just r)) []
       in
       (select (CursorSelection 1 3 3 6) (Ast ast)) 
       @?= 
       singleton (r,[0,0]) 

t3 = testCase "return root if successor does not match" $ 
       let r = SrcSpan 1 1 19 7 
           ast = (Node (AstNode "a" (Just r)) [c])  
           c =  Node (AstNode "b" (Just (SrcSpan 10 2 17 9 ))) []
       in
       (select (CursorSelection 1 2 3 9) (Ast ast)) 
       @?= 
       singleton (r,[0]) 

t4 = testCase "return leaf in three containing spans" $ 
       let r = SrcSpan 2 1 4 2
           ast = (Node (AstNode "a" (Just (SrcSpan 1 1 16 3))) [c1])  
           c1 =  Node (AstNode "b" (Just (SrcSpan 1 1 5 9))) [c2]
           c2 =  Node (AstNode "b" (Just r)) []
       in
       (select (CursorSelection 2 1 3 1) (Ast ast)) 
       @?= 
       singleton (r,[0,0,0]) 


t5 = testCase "triangle, select the correct child" $ 
       let r = SrcSpan 2 1 4 2
           ast = (Node (AstNode "a" (Just (SrcSpan 1 1 16 3))) [c1,c2])  
           c1 =  Node (AstNode "b" (Just (SrcSpan 10 1 15 9))) []
           c2 =  Node (AstNode "b" (Just r)) []
       in
       (select (CursorSelection 2 1 3 1) (Ast ast)) 
       @?= 
       singleton (r,[0,1]) 

t6 = testCase "triangle, select multiple locations" $ 
       let r = SrcSpan 2 1 4 2
           ast = Node (AstNode "a" (Just (SrcSpan 1 1 16 3))) [c1,c2]  
           c1 =  Node (AstNode "b" (Just (SrcSpan 10 1 15 9))) []
           c2 =  Node (AstNode "b" (Just r)) [c3]
           c3 =  Node (AstNode "b" (Just r)) []
       in
       (toList $ select (CursorSelection 2 1 3 1) (Ast ast))
       @?= 
       [(r,[0,1]),(r,[0,1,0])] 
{-
c1 = testCase "1" $ (SrcSpan 1 1 10 1) > (SrcSpan 3 1 7 10) @?= True
c2 = testCase "1" $ (SrcSpan 1 9 10 2) > (SrcSpan 1 11 10  1) @?= True
-}
