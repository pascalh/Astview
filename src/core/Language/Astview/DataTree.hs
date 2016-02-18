{-| This module contains datatype-generic functions to gain a 'Ast'
out of an arbitrary term.
-}
module Language.Astview.DataTree
  (annotateWithPaths
  ,dataToAstIgnoreByExample
  ,flatten
  ,dataToAst
  ,dataToAstSimpl
  ,removeSubtrees
  ,manual
  ) where

import Data.Generics (Data
                     ,extQ
                     ,gmapQ
                     ,showConstr
                     ,toConstr)
import Data.Typeable
import Data.Tree (Tree(..))
import Data.Maybe(isNothing,isJust)
import Language.Astview.Language

-- |uses the tree builder to get a tree and then runs the usual update and
-- path annotation functions.
--
-- 'manual' is a more flexible version of 'dataToAst', but in most cases
-- 'dataToAst' is the function of choice.
-- Use 'manual' only if the tree builder function has to differ from the default
-- defined in 'dataToAst' (see 'Language.Astview.Languages.HaskellCore' for an
-- example).
manual :: (t -> Tree (Maybe AstNode)) -- ^tree builder
       -> t
       -> Ast
manual f =
  Ast . annotateWithPaths . delegateSrcLoc . removeSubtrees isEmpty . removeNothings . f

-- |creates an 'Ast' from the input term by using the constructor names
-- of node labels and keeping the structure. We try to associate source
-- locations to nodes by applying the source location selector at every subtree.
-- We ignore a whole subtree if the filter predicate holds (useful if meta Data
-- like source locations should not be visible in the 'Ast').
dataToAst :: (Data t)
  => (forall span.Data span => span -> Maybe SrcSpan) -- ^ source location selector
  -> (forall st. Typeable st => st -> Bool) -- ^ filter predicate
  -> t -> Ast
dataToAst getSrcLoc pIgnore t = manual worker t where

  worker :: (Data t,Typeable t) => t -> Tree (Maybe AstNode)
  worker term | pIgnore term = Node Nothing []
              | otherwise    = (gdefault `extQ` atString) term where

      atString :: String -> Tree (Maybe AstNode)
      atString s = Node (Just $ AstNode s Nothing [] Identificator) []

      gdefault :: (Typeable t,Data t) => t -> Tree (Maybe AstNode)
      gdefault x = Node (Just n) cs where

        n :: AstNode
        n = AstNode (showConstr $ toConstr x) (getSrcLoc x) [] Operation

        cs = gmapQ worker x

-- |usually we want to discard all values of one type (the type of term
-- annotations) from the ast. Just give one example value of the annotation
-- type to this function and all values of this type are being discarded from
-- the ast.
dataToAstIgnoreByExample :: (Data t,Typeable t,Typeable ig,Data ig)
       => (forall a . (Data a,Typeable a)  => a -> Maybe SrcSpan) -- ^ source location selector
       -> ig -- ^all values of this type will be missing in 'Ast'
       -> t -> Ast
dataToAstIgnoreByExample getLoc igExample = dataToAst getLoc ignore where
  ignore t = equalTypes t igExample

-- |dataToAstSimpl stores the terms' structure directly in the 'Ast' (without
-- annotating source locations or discarding subtrees)
dataToAstSimpl :: Data t => t -> Ast
dataToAstSimpl = dataToAst (const Nothing) (const False)

-- * helper functions

-- |Every node will be annotated with its path, beginning with @[0]@ for root
-- node. Don't manually annotate paths during creation of 'Ast's, instead
-- use a default (e.g. @[]@) and add paths by 'annotateWithPaths' afterwards.
annotateWithPaths :: Tree AstNode -> Tree AstNode
annotateWithPaths = f [0] where
  f :: [Int] -> Tree AstNode -> Tree AstNode
  f p (Node (AstNode l s _ t) cs) =
    Node (AstNode l s p t) $ zipWith (\i c -> f (p++[i]) c) [0,1..] cs

removeNothings :: Tree (Maybe AstNode) -> Tree AstNode
removeNothings (Node Nothing _)   = error "cannot remove the root of a one-noded tree"
removeNothings (Node (Just n) cs) =
  Node n (map removeNothings $ filter (isJust . rootLabel) cs)

-- |removes all proper subtrees satisfying the predicate.
-- The predicate is not being checked at root node, since the resulting tree
-- has to be non-empty.
removeSubtrees :: (Tree a -> Bool) -> Tree a -> Tree a
removeSubtrees p (Node n cs) = Node n $ map (removeSubtrees p) $ filter (not . p) cs

isEmpty :: Tree AstNode -> Bool
isEmpty (Node (AstNode "" _ _ _) []) = True
isEmpty _            = False


-- |returns whether both values are of the same type
equalTypes :: (Typeable b1,Typeable b2)  => b1 -> b2  -> Bool
equalTypes t1 t2 = typeOf t1 == typeOf t2

-- |transform nested usage of cons operator to one flat operation
-- (this drastically reduces the asts depth)
flatten :: Ast -> Ast
flatten (Ast t) = Ast (annotateWithPaths $ flat t) where

  flat :: Tree AstNode -> Tree AstNode
  flat t@(Node _ []) = t
  flat t@(Node (AstNode "(:)" s p Operation) _) =
    let lbl = '[': replicate (length (collect t) - 1) ',' ++ "]" in
    Node (AstNode lbl s p Operation)  (collect t)
  flat (Node n cs) = Node n $ map flat cs

  collect :: Tree AstNode -> [Tree AstNode]
  collect (Node (AstNode "(:)" _ _ Operation) cs) = case cs of
    [t1,t2] -> flat t1 : collect t2
    _          -> err
  collect (Node (AstNode "[]" _ _ Operation) [])       = []
  collect _                                            = err

  err = error "Malformed term. Disabling flattening solves the problem."

-- |delegates source location annotation to unique subtrees. This should
-- not lead to malformed syntax trees, since every unique subtree which is not
-- annotated with a source location represents the same source location as its
-- direct predecessor. If a subtree represents a smaller source location than
-- its predecessor, the subtree has to be explicitly tagged with
-- a smaller source location (in this case no delegation takes place).
delegateSrcLoc :: Tree AstNode -> Tree AstNode
delegateSrcLoc t@(Node (AstNode _ (Just srcLoc) _ _) [Node n cs]) =
  t { subForest = [Node n'(map delegateSrcLoc cs)] } where

    n' :: AstNode
    n' | isNothing (srcspan n) = n  { srcspan = Just srcLoc }
       | otherwise            = n

delegateSrcLoc (Node n cs) = Node n (map delegateSrcLoc cs)
