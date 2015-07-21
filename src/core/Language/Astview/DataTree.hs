{-| This module contains datatype-generic functions to gain a 'Ast'
out of an arbitrary term.
-}
module Language.Astview.DataTree 
  (data2Ast
  ,data2AstHo
  ,annotateWithPaths
  ,data2AstHoIg
  ,flatten
  ,data2AstOpt
  ,removeSubtrees
  ) where

-- syb
import Data.Generics (Data
                     ,extQ
                     ,gmapQ
                     ,showConstr
                     ,toConstr)
import Data.Typeable
import Data.Tree (Tree(..))
import Data.Maybe(isNothing)
import Language.Astview.Language

data2AstOpt :: (Data t) => (forall srcloc.Data srcloc => srcloc -> Maybe SrcLocation) 
                        ->  (forall st . Typeable st => st -> Bool) 
                        -> t -> Ast
data2AstOpt getSrcLoc pIgnore = Ast . annotateWithPaths . delegateSrcLoc . removeSubtrees isEmpty . removeNothings . worker where

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



-- |Trealise Data to Tree (from SYB 2, sec. 3.4 )
data2treeHO :: (Data t) => (forall a . Data a => a -> Maybe SrcLocation)
                        -> t -> Tree AstNode
data2treeHO f = gdefault `extQ` atString where

  atString :: String -> Tree AstNode
  atString s = Node (AstNode s Nothing [] Identificator) []

  gdefault :: Data t => t -> Tree  AstNode
  gdefault x = Node (AstNode (showConstr $ toConstr x) (f x) [] Operation )
                    (gmapQ (data2treeHO f) x)

-- |see 'data2AstHo' but no 'SrcLocation's will be annotated
data2Ast :: Data t => t -> Ast
data2Ast = data2AstHo (const Nothing)

-- |gains a 'SrcLocation'-selection function an a term. Returns the 'Ast'
-- which is completely annotated (i.e. all fields of type 'AstNode' will
-- hold values).
data2AstHo :: Data t => (forall a . Data a => a -> Maybe SrcLocation) -> t -> Ast
data2AstHo f = Ast . annotateWithPaths . data2treeHO f

-- |every node will be annotated with its path.
annotateWithPaths :: Tree AstNode -> Tree AstNode
annotateWithPaths = f [0] where
  f :: [Int] -> Tree AstNode -> Tree AstNode
  f p (Node (AstNode l s _ t) cs) =
    Node (AstNode l s p t) $ zipWith (\i c -> f (p++[i]) c) [0,1..] cs



-- |same as data2AstHo, but ignoring all values of type b during the creation.
-- Thus it can be used to remove annotations from a tree.
data2AstHoIg :: (Data t,Typeable t,Typeable b,Data b)
       => (forall a . (Data a,Typeable a)  => a -> Maybe SrcLocation)
       -> b        -> t -> Ast
data2AstHoIg f marker = Ast . annotateWithPaths . delegateSrcLoc . removeNothings . worker where

  worker :: (Data t,Typeable t)
         => t -> Tree (Maybe AstNode)
  worker term
   | term `equalTypes` marker = Node Nothing []
   | otherwise                = (gdefault `extQ` atString) term where

      atString :: String -> Tree (Maybe AstNode)
      atString s = Node (Just $ AstNode s Nothing [] Identificator) []

      gdefault :: (Typeable t,Data t) => t -> Tree (Maybe AstNode)
      gdefault x = Node (Just n) cs where

        n :: AstNode
        n = AstNode (showConstr $ toConstr x) (f x) [] Operation

        cs = gmapQ worker x

removeNothings :: Tree (Maybe AstNode) -> Tree AstNode
removeNothings (Node Nothing _)   = error "cannot remove the root of a one-noded tree"
removeNothings (Node (Just n) cs) = Node n (map removeNothings $ filter isJustNode cs)

-- |removes all proper subtrees satisfying the predicate. 
-- The predicate is not being checked at root node, since the resulting tree
-- has to be non-empty. 
removeSubtrees :: (Tree a -> Bool) -> Tree a -> Tree a
removeSubtrees p (Node n cs) = Node n $ map (removeSubtrees p) $ filter (not . p) cs

isEmpty :: Tree AstNode -> Bool
isEmpty (Node (AstNode "" _ _ _) []) = True
isEmpty _            = False

isJustNode :: Tree (Maybe a) -> Bool
isJustNode (Node (Just _) _) = True
isJustNode (Node Nothing _ ) = False


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
    n' | isNothing (srcloc n) = n  { srcloc = Just srcLoc }
       | otherwise            = n

delegateSrcLoc (Node n cs) = Node n (map delegateSrcLoc cs)
