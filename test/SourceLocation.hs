{-| This module contains test cases for functions on source locations. -}
module SourceLocation(testSourceLocations) where
import Test.HUnit 

import Data.Tree

import Language.Astview.GUIData
import Language.Astview.SourceLocation
import Language.Astview.Language
import Language.Astview.DataTree(annotateWithPaths)

-- * contains

testSourceLocations :: Test
testSourceLocations = 
  TestLabel "Source locations" $ TestList [groupContains,groupSelect]

groupContains :: Test
groupContains = 
  TestLabel "ord instance for source locations implements contains" $ TestList
            [groupInOneline
            ,groupSorrounded
            ,groupSameBegin
            ,groupSameEnd
            ,groupEx
            ]

groupInOneline :: Test
groupInOneline = TestLabel "Everything in one line" $ TestList

  [ SrcSpan 3 1 3 2 > SrcSpan 3 1 3 2 ~?= False 
  , SrcSpan 4 1 4 9 > SrcSpan 4 3 4 6 ~?= True
  , SrcSpan 4 0 4 6 < SrcSpan 4 1 4 9 ~?= False 
  , SrcSpan 4 2 4 16 < SrcSpan 4 1 4 9 ~?= False 
  , SrcSpan 4 1 4 9 > SrcSpan 4 1 4 9 ~?= False 
  ]


groupSorrounded :: Test
groupSorrounded = TestLabel "Point sorrounded by span" $ TestList 

  [ SrcSpan 4 9 7 9 > SrcSpan 5 18 6 100 ~?= True 
  , SrcSpan 4 9 7 9 > SrcSpan 5 1 6 1 ~?= True 
  ]


groupSameBegin :: Test
groupSameBegin = TestLabel "Same begin line" $ TestList 

  [ SrcSpan 4 9 7 9 > SrcSpan 4 18 6 100 ~?= True 
  , SrcSpan 4 9 6 9 > SrcSpan 4 18 7 100 ~?= False 
  ]

groupSameEnd :: Test
groupSameEnd = TestLabel "Same end line" $ TestList 

  [ SrcSpan 4 9 7 9 > SrcSpan 5 18 7 5 ~?= True 
  , SrcSpan 1 9 7 9 > SrcSpan 4 18 7 10 ~?= False 
  ]


groupEx :: Test
groupEx = TestLabel "Extreme cases" $ TestList

  [ "equal position" ~: SrcSpan 1 9 7 9 > SrcSpan 1 9 7 9 ~?= False 
  , "same end"       ~: SrcSpan 1 1 7 9 > SrcSpan 1 2 7 9 ~?= True 
  , "same begin"     ~: SrcSpan 1 9 7 9 > SrcSpan 1 9 7 3 ~?= True 
  ]


-- * select

groupSelect :: Test
groupSelect = TestLabel "selecting the greatest sorrounding source location" $ 
  TestList [t1,t2,t3,t4,t5,t6]

mkTree :: String -> SrcLocation -> [Tree AstNode] -> Tree AstNode
mkTree l s cs = annotateWithPaths $ Node (AstNode l (Just s) [] Identificator) cs

t1 :: Test
t1 = "return first occourence"  ~: 
       toList (select (CursorSelection 1 2 1 7) (Ast ast)) ~?= [(SrcSpan 1 2 1 7,[0])]  where
          ast = mkTree "a" (SrcSpan 1 2 1 7) []

t2 :: Test
t2 = "return immediate successor" ~:
       let r = SrcSpan 1 2 3 9
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c]
           c =  mkTree "b" r []
       in
       select (CursorSelection 1 3 3 6) (Ast ast) 
       ~?= 
       singleton (r,[0,0]) 

t3 :: Test
t3 = "return root if successor does not match" ~: 
       let r = SrcSpan 1 1 19 7 
           ast = mkTree "a" r [c]  
           c =  mkTree "b" (SrcSpan 10 2 17 9 ) []
       in
       select (CursorSelection 1 2 3 9) (Ast ast) 
       ~?= 
       singleton (r,[0]) 

t4 :: Test
t4 = "return leaf in three containing spans" ~: 
       let r = SrcSpan 2 1 4 2
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c1]
           c1 =  mkTree "b" (SrcSpan 1 1 5 9) [c2]
           c2 =  mkTree "b"  r []
       in
       select (CursorSelection 2 1 3 1) (Ast ast) 
       ~?= 
       singleton (r,[0,0,0]) 

t5 :: Test
t5 = "triangle, select the correct child" ~: 
       let r = SrcSpan 2 1 4 2
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c1,c2]  
           c1 =  mkTree "b" (SrcSpan 10 1 15 9) []
           c2 =  mkTree "b" r []
       in
       select (CursorSelection 2 1 3 1) (Ast ast) 
       ~?= 
       singleton (r,[0,1]) 

t6 :: Test
t6 = "triangle, select multiple locations" ~: 
       let r = SrcSpan 2 1 4 2
           ast = mkTree "a" (SrcSpan 1 1 16 3) [c1,c2]  
           c1 =  mkTree "b" (SrcSpan 10 1 15 9) []
           c2 =  mkTree "b" r [c3]
           c3 =  mkTree "b" r []
       in
       select (CursorSelection 2 1 3 1) (Ast ast)
       ~?= 
       ins (r,[0,1]) (singleton (r,[0,1,0]))
