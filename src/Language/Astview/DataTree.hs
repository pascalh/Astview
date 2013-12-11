{-| This module contains datatype-generic functions to gain a 'Ast' 
out of an arbitrary term.
-}
module Language.Astview.DataTree (data2Ast,data2AstHo,annotateWithPaths,data2AstHoIg) where

-- syb
import Data.Generics (Data
                     ,Typeable(..)
                     ,extQ
                     ,gmapQ
                     ,showConstr
                     ,toConstr)
import Data.Tree (Tree(Node))
import Language.Astview.Language

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
data2AstHoIg f marker = Ast . annotateWithPaths . removeNothings . worker where

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

isJustNode :: Tree (Maybe a) -> Bool 
isJustNode (Node (Just _) _) = True 
isJustNode (Node Nothing _ ) = False 


-- |returns whether both values are of the same type
equalTypes :: (Typeable b1,Typeable b2)  => b1 -> b2  -> Bool
equalTypes t1 t2 = typeOf t1 == typeOf t2
