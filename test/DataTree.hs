{-| This module contains test cases for the functions creating abstract
syntax trees defined in module 'Language.Astview.DataTree'. -}
module DataTree(testDataTree) where
import Prelude hiding (span)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (label)

import Data.Tree(Tree(..))
import Control.Monad(liftM,liftM2,liftM3)
import Data.Foldable (foldrM)
import Data.List(sort)
import Data.Generics (Data)
import Data.Typeable(Typeable,typeOf)
import Control.Monad.Omega(diagonal)
import Language.Astview.DataTree
import Language.Astview.Language


propIgnoreInt :: TestTree
propIgnoreInt = testProperty "removing leafs with int values" p where

  p (TermUnit t) = act t === exped t

  act :: Term () -> Tree String
  act = fmap label . ast . dataToAstIgnoreByExample (const Nothing) (1::Int)

  exped :: Term () -> Tree String
  exped = removeSubtrees (\t -> isNumber t || isEmpty t) . fmap label . ast . dataToAstSimpl

  isEmpty :: Tree String -> Bool
  isEmpty = null . rootLabel

  isNumber :: Tree String -> Bool
  isNumber (Node ('-':xs)  _) = not (null xs) && all (\x -> elem x ['0'..'9']) xs
  isNumber (Node xs@(_:_)  _) = all (\x -> elem x ['0'..'9']) xs
  isNumber _                  = False

-- *

testDataTree :: TestTree
testDataTree = testGroup "data to ast" [testsBasic,propIgnoreInt] where

testsBasic :: TestTree
testsBasic = testGroup "Basic data to ast transformations"
                       [mkProp b f | b <- [True,False] , f <- [True,False]] where

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
  mkAst = if b then dataToAstSimpl
               else dataToAstIgnoreByExample (const Nothing) ()

-- * a toy language

data Term ann
  = EmptyLeaf
  | Leaf ann String
  | Number ann Int
  | Nested ann (Term ann)
  | BinaryBranch ann (Term ann) (Term ann)
  | List ann [Term ann]
  deriving (Show,Data,Typeable,Functor)

newtype TermUnit = TermUnit {term :: Term () }

instance Show TermUnit where
  show = show . term

instance Arbitrary TermUnit where
  arbitrary = liftM TermUnit arbitrary

newtype TermSrcloc = TermSrcloc { termSrcLoc :: Term SrcSpan}

instance Show TermSrcloc where
  show = show . termSrcLoc

instance Arbitrary TermSrcloc where
  arbitrary = do
    TermUnit t <- arbitrary
    l <- arbitrary
    t' <- arbitrarySrcLocs l t
    return $ TermSrcloc t'

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
arbitraryTerm n
  | n > 0 = oneof [ arbitraryNested n, arbitraryBinary n, arbitraryList n]
  | n < 0 = return EmptyLeaf

arbitraryString :: Arbitrary a => Gen (Term a)
arbitraryString = liftM2 Leaf arbitrary (listOf1 arbitraryChar) where

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

-- |split n returns an arbitrary pair (x1,x2) such that x1 + x2 == n
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

-- |given a term and a source span arbitrarySrcLocs annotates the term
-- with source spans, such that the span at the root of every subterm t
-- surrounds the spans of every successor of t.
arbitrarySrcLocs :: SrcSpan -> Term () -> Gen (Term SrcSpan)
arbitrarySrcLocs _ EmptyLeaf = return EmptyLeaf
arbitrarySrcLocs l (Leaf _ str) = return $ Leaf l str
arbitrarySrcLocs l (Number _ n) = return $ Number l n
arbitrarySrcLocs l (Nested _ t) = do
  l' <- sublocation l
  t' <- arbitrarySrcLocs l' t
  return $ Nested l' t'

-- |sublocation loc returns a arbitrary source span loc' which is
-- surrounded by loc
sublocation :: SrcSpan -> Gen SrcSpan
sublocation loc = do
  return loc

-- |returns n source positions which are in the given source span
positionsInSpan :: Int -> SrcSpan -> [SrcPos]
positionsInSpan n = sort . take n . diagonal . locsPerLine where

  locsPerLine :: SrcSpan -> [[SrcPos]]
  locsPerLine (SrcSpan (SrcPos bl bc) (SrcPos el ec))
    | bl == el = [[SrcPos bl c | c <- [bc..ec]]]
    | bl > el = []
    | bl < el = [SrcPos bl c | c <- [bc..]] :
                locsPerLine (span (bl+1) 1 el ec)


