{-| This module contains datatype-generic functions to gain a 'Ast' 
out of an arbitrary term.
-}
module Language.Astview.DataTree (data2Ast,data2AstHo,annotateWithPaths) where

-- syb
import Data.Generics (Data
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
