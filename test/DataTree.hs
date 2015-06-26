{-| This module contains test cases for the functions creating abstract 
syntax trees defined in module 'Language.Astview.DataTree'. -}
module DataTree(testDataTree) where

import Test.Tasty
import Test.Tasty.QuickCheck hiding (label)

import Data.Tree(Tree(..))
import Control.Monad(liftM,liftM2,liftM3)
import Data.Foldable (foldrM)
import Data.Generics (Data)
import Data.Typeable(Typeable)
import Language.Astview.DataTree 
import Language.Astview.Language

testDataTree :: TestTree
testDataTree = testGroup "Data to ast" properties where

  properties = [mkProp b f | b <- [True,False] , f <- [True,False]]

mkProp ::Bool -> Bool -> TestTree
mkProp b f = testProperty name p where

  name = (if b then " " else "remove annotations ") ++ 
         (if f then "flattened" else "")

  p :: TermUnit -> Property
  p (TermUnit t) = actual b f t === expected b f t

-- |applies functions from 'Language.Astview.DataTree' to given term
actual :: Bool  -- ^ should the term contain annotations=
       -> Bool  -- ^ should be term be flattened
       -> Term () -- ^ input term
       -> Tree String
actual b f = fmap label . ast . mkFlat . mkAst  where
 
  mkFlat :: Ast -> Ast 
  mkFlat = if f then flatten else id

  mkAst :: Term () -> Ast 
  mkAst = if b then data2Ast else data2AstHoIg (const Nothing) () 

-- * a toy language

data Term ann
  = EmptyLeaf 
  | Leaf ann String
  | Number ann Int
  | Nested ann (Term ann)
  | BinaryBranch ann (Term ann) (Term ann)
  | List ann [Term ann]
  deriving (Show,Data,Typeable)

newtype TermUnit = TermUnit {term :: Term () } 

instance Show TermUnit where
  show = show . term

instance Arbitrary TermUnit where
  arbitrary = liftM TermUnit arbitrary

-- * creating the expected trees

-- |builds the expected tree for a given term
expected :: Bool -- ^should tree contain annotations
     -> Bool -- ^flatten lists in tree?
     -> Term () -> Tree String
expected _ _ EmptyLeaf = Node "EmptyLeaf" []
expected b _ (Leaf _ s)  = annotate b $ Node "Leaf" [Node s []]
expected b _ (Number _ n) = annotate b $ Node "Number" [Node (show n) []]
expected b f (Nested _ t) = annotate b $ Node "Nested" [expected b f t]
expected b f (BinaryBranch _ t1 t2) = 
  annotate b $ Node "BinaryBranch" [expected b f t1,expected b f t2]
expected b False (List _ ts) = annotate b $ Node "List" [ts' ts] where

  ts' :: [Term ()] -> Tree String
  ts' []     = Node "[]" []
  ts' (t:ts) = Node "(:)" [expected b False t,ts' ts]

expected b True (List _ []) = annotate b $ Node "List" [Node "[]" []]
expected b True (List _ ts) = annotate b $ Node "List" [Node lbl (map (expected b True) ts)] where

  lbl :: String
  lbl = "["++replicate (length ts - 1) ',' ++ "]"

annotate :: Bool -> Tree String -> Tree String
annotate b (Node l cs) 
  | b         = let unit = Node "()" [] 
                in Node l $ unit : cs
  | otherwise = Node l cs

-- ** the generation of arbitrary terms

instance Arbitrary ann => Arbitrary (Term ann) where
  arbitrary = sized arbitraryTerm

arbitraryTerm :: Arbitrary a => Int -> Gen (Term a)
arbitraryTerm 0 = oneof [return EmptyLeaf, arbitraryString, arbitraryInt]
arbitraryTerm n | n > 0 =
  oneof [ arbitraryNested n, arbitraryBinary n, arbitraryList n]

arbitraryString :: Arbitrary a => Gen (Term a)
arbitraryString = liftM2 Leaf arbitrary (listOf arbitraryChar) where
  
  arbitraryChar :: Gen Char
  arbitraryChar = elements $ ['A'..'Z']++['a'..'z']

arbitraryInt :: Arbitrary a => Gen (Term a)
arbitraryInt = liftM2 Number arbitrary arbitrary 

arbitraryNested :: Arbitrary a => Int -> Gen (Term a)
arbitraryNested n = liftM2 Nested arbitrary (arbitraryTerm (n-1))

arbitraryBinary :: Arbitrary a => Int -> Gen (Term a)
arbitraryBinary n = do
  (n1,n2) <- split n
  liftM3 BinaryBranch arbitrary (arbitraryTerm n1) (arbitraryTerm n2)

arbitraryList :: Arbitrary a => Int -> Gen (Term a)
arbitraryList n = do
  m <- elements [0..5]
  ann <- arbitrary
  case m of
    0 -> return $ List ann []
    1 -> liftM (\t -> List ann [t]) (arbitraryTerm (n-1)) 
    _ -> do
      ns <- splits m n
      ts <- foldrM (\n acc -> do {t <- arbitraryTerm n ;  return $ t:acc}) [] ns
      return $ List ann ts

-- |returns an arbitrary pair (x1,x2) such that x1 + x2 == n
split :: Int -> Gen (Int,Int)
split n = do
  x <- elements [0..n] 
  return (x,n-x)

splits :: Int -> Int -> Gen [Int]
splits 2 n = do 
  (x1,x2) <- split n  
  return [x1,x2]
splits m n = do
  (x1,x2) <- split n
  xs <- splits (m-1) x1
  return (x2:xs)


