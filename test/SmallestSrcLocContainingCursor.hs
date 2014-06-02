module SmallestSrcLocContainingCursor (testSelect) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Astview.Pathlist
import Language.Astview.SmallestSrcLocContainingCursor 
  (smallestSrcLocContainingCursorPos) 
import Language.Astview.DataTree(annotateWithPaths)
import Language.Astview.Language

import Data.Tree (Tree(Node))

testSelect :: TestTree
testSelect = 
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
