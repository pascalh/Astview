module Main where
import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit 


import Language.Astview.GUIData
import Language.Astview.SourceLocation

main :: IO ()
main = defaultMain tests
 
tests =
  [
    testGroup "simple src lops"
    [
       testCase "One out of one" testFindOneOutOfOne 
    ,
       testCase "Selection out of one span (too big)" 
                testSelectionOutOfOneSingleSpan1 
    ,
       testCase "Selection out of one span (too small)" 
                testSelectionOutOfOneSingleSpan2 
    ,
       testCase "Selection out of one span, overlapping " 
                testSelectionOutOfOneSingleOverlapping 
    ]
  ]

testFindOneOutOfOne =  
  let srcSpan = SrcSpan 1 1 3 1  
  in findSrcLoc (CursorSelection 1 1 3 1) [srcSpan] @?= Just srcSpan 

testSelectionOutOfOneSingleSpan1 =  
  findSrcLoc (CursorSelection 4 1 4 3) [SrcSpan 1 1 1 2] @?= Nothing

testSelectionOutOfOneSingleSpan2 =  
  findSrcLoc (CursorSelection 4 1 4 7) [SrcSpan 1 1 1 2] @?= Nothing

testSelectionOutOfOneSingleOverlapping =  
  findSrcLoc (CursorSelection 4 1 4 7) [SrcSpan 4 3 4 12] @?= Nothing
