{-#Language RankNTypes #-}
module Haskell where

-- local imports
import Language.Astview.Language 
import Language.Astview.SourceLocation (addPaths)

import qualified Language.Haskell.Exts.Parser as HsParser 
import Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.SrcLoc as HsSrcLoc

import Data.Generics (Data
                     ,showConstr
                     ,toConstr)
import Data.Tree(Tree(..))

haskellexts :: Language 
haskellexts = Language 
  "Haskell" 
  "Haskell" 
  [".hs",".lhs"] 
  parsehs 

astnode :: String -> Maybe SrcLocation -> AstNode 
astnode l s = AstNode l s []

parsehs :: String -> Either Error Ast 
parsehs s = case HsParser.parse s :: HsParser.ParseResult (Module HsSrcLoc.SrcSpan) of
    HsParser.ParseOk t   -> Right $ Ast $ addPaths $ toAst t 
    HsParser.ParseFailed (HsSrcLoc.SrcLoc _ l c) m -> 
      Left $ ErrLocation (SrcPosition l c) m


pair' :: (t1 -> Tree AstNode) -> (t2 -> Tree AstNode) -> (t1,t2) -> Tree AstNode
pair' k1 k2 (x,y) = Node (astnode "(,)" Nothing) [k1 x,k2 y]

mb' :: (t -> Tree AstNode) -> Maybe t -> Tree AstNode
mb' _ Nothing = Node (astnode "Nothing" Nothing) []
mb' k  (Just t)= Node (astnode "Just" Nothing) [k t]

mb :: ToAst f => Maybe (f HsSrcLoc.SrcSpan) -> Tree AstNode
mb Nothing = Node (astnode "Nothing" Nothing) []
mb (Just t)= Node (astnode "Just" Nothing) [toAst t]

list :: ToAst f => [f HsSrcLoc.SrcSpan] -> Tree AstNode
list [] = Node (astnode "[]" Nothing) []
list (x:xs) = Node (astnode "(:)" Nothing) [toAst x , list xs]


list' :: (t -> Tree AstNode) -> [t] -> Tree AstNode
list' _ [] = Node (astnode "[]" Nothing) []
list' k (x:xs) = Node (astnode "(:" Nothing) [k x,list' k xs] 

toSrcLocHs :: HsSrcLoc.SrcSpan -> SrcLocation
toSrcLocHs (HsSrcLoc.SrcSpan _ c1 c2 c3 c4) = SrcSpan c1 c2 c3 c4

class (Data (f HsSrcLoc.SrcSpan),Annotated f) => ToAst f where
  toAstNode :: f HsSrcLoc.SrcSpan -> AstNode
  toAstNode t = astnode (showConstr $ toConstr t) (Just $ toSrcLocHs $ ann t)  

  toAst :: f HsSrcLoc.SrcSpan -> Tree AstNode

instance ToAst ModuleName where
  toAst t@(ModuleName _ s) = Node (toAstNode t) [str s]

instance ToAst SpecialCon where
    toAst t@(UnitCon _             )= Node (toAstNode t) []
    toAst t@(ListCon _             )= Node (toAstNode t) []
    toAst t@(FunCon _             )= Node (toAstNode t) []
    toAst t@(TupleCon _  x i)= Node (toAstNode t) [leaf x,leaf i]
    toAst t@(Cons _                )= Node (toAstNode t) []
    toAst t@(UnboxedSingleCon _    )= Node (toAstNode t) []

instance ToAst QName where
    toAst t@(Qual _ x0 x1 )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(UnQual _                x0 )= Node (toAstNode t) [toAst x0]
    toAst t@(Special _ x0          )= Node (toAstNode t) [toAst x0]

instance ToAst Name where
    toAst t@(Ident _ s)= Node (toAstNode t) [str s]
    toAst t@(Symbol _ s)= Node (toAstNode t) [str s]

instance ToAst IPName where
    toAst t@(IPDup _ s)= Node (toAstNode t) [str s]
    toAst t@(IPLin _ s)= Node (toAstNode t) [str s]

instance ToAst QOp where
    toAst t@(QVarOp _ x0 )= Node (toAstNode t) [toAst x0]
    toAst t@(QConOp _ x0 )= Node (toAstNode t) [toAst x0]

instance ToAst Op where
    toAst t@(VarOp _ x0    )= Node (toAstNode t) [toAst x0]
    toAst t@(ConOp _ x0    )= Node (toAstNode t) [toAst x0]

instance ToAst CName where
    toAst t@(VarName _ x0 )= Node (toAstNode t) [toAst x0]
    toAst t@(ConName _ x0 )= Node (toAstNode t) [toAst x0]


instance ToAst Module where
    toAst t@(Module _ x0 xs0 xs1 xs2)= Node (toAstNode t) [mb x0,list xs0,list xs1,list xs2]
    toAst t@(XmlPage _ x0 xs0 x1 xs1 mb0 xs2)= 
      Node (toAstNode t) [toAst x0,list xs0, toAst x1,list xs1,mb mb0, list xs2]
    toAst t@(XmlHybrid _ mb0 xs0 xs1 xs2 x0 xs3 mb1 xs4)= 
      Node (toAstNode t) [mb mb0,list xs0,list xs1,list xs2,toAst x0,list xs3,mb mb1,list xs4]

instance ToAst XAttr where
  toAst t@(XAttr _ x0 x1) = Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst ModuleHead where
  toAst t@(ModuleHead _ x0 mb0 mb1 )= Node(toAstNode t) [toAst x0,mb mb0,mb mb1]

instance ToAst ExportSpecList where
    toAst t@(ExportSpecList _ xs0)= Node (toAstNode t) [list xs0]

instance ToAst ExportSpec where
     toAst t@(EVar _ x0                 )= Node (toAstNode t) [toAst x0]
     toAst t@(EAbs _ x0                 )= Node (toAstNode t) [toAst x0]
                                        
     toAst t@(EThingAll _ x0            )= Node (toAstNode t) [toAst x0]
                                        
     toAst t@(EThingWith _ x0 xs0 )= Node (toAstNode t) [toAst x0,list xs0]
                                        
     toAst t@(EModuleContents _ x0 )= Node (toAstNode t) [toAst x0]

instance ToAst ImportDecl where
  toAst t@(ImportDecl _ x0 b0 b1 mb0 mb1 mb2) = Node (toAstNode t) [toAst x0,leaf b0,leaf b1,mb' str mb0,mb mb1,mb mb2]

instance ToAst ImportSpecList where
    toAst t@(ImportSpecList _ b xs0)= Node (toAstNode t) [leaf b,list xs0]

instance ToAst ImportSpec where
     toAst t@(IVar _ x0                  )= Node (toAstNode t) [toAst x0]
     toAst t@(IAbs _ x0                  )= Node (toAstNode t) [toAst x0]
                                        
     toAst t@(IThingAll _ x0             )= Node (toAstNode t) [toAst x0]
                                        
     toAst t@(IThingWith _ x0 xs0  )= Node (toAstNode t) [toAst x0,list xs0]
                                        

instance ToAst Assoc where
     toAst t@(AssocNone _ )= Node (toAstNode t) []
     toAst t@(AssocLeft _ )= Node (toAstNode t) []
     toAst t@(AssocRight _ )= Node (toAstNode t) []


instance ToAst DataOrNew where
  toAst t@(DataType _) = Node (toAstNode t) []
  toAst t@(NewType _) = Node (toAstNode t) []

instance ToAst Deriving where
  toAst t@(Deriving _ xs0) = Node (toAstNode t) [list xs0]

instance ToAst Match where
  toAst t@(Match _ x0 xs0 x1 mb0) = Node (toAstNode t) [toAst x0,list xs0,toAst x1,mb mb0]
  toAst t@(InfixMatch _ x0 x1 xs0 x2 mb0) = Node (toAstNode t) [toAst x0,toAst x1,list xs0,toAst x2,mb mb0]

instance ToAst Decl where
     toAst t@(TypeDecl _ x0 x1)= Node (toAstNode t) [toAst x0,toAst x1]
     toAst t@(TypeFamDecl _ x0 mb0)= Node (toAstNode t) [toAst x0,mb mb0]
     toAst t@(DataDecl _ x0 mb0 x1                  xs0 mb1)= Node (toAstNode t) [toAst x0,mb mb0,toAst x1,list xs0,mb mb1]
     
     toAst t@(GDataDecl _ x0 mb0 x1 mb1 xs0 mb2)= Node (toAstNode t) [toAst x0,mb mb0,toAst x1,mb mb1,list xs0,mb mb2]
     
     toAst t@(DataFamDecl _ mb0 x0 mb1      )= Node (toAstNode t) [mb mb0,toAst x0,mb mb1]
     
     toAst t@(TypeInsDecl _ x0 x1)= Node (toAstNode t) [toAst x0,toAst x1]
     toAst t@(DataInsDecl _ x0 x1                  xs0 mb0)= Node (toAstNode t) [toAst x0,toAst x1,list xs0,mb mb0]
     
     toAst t@(GDataInsDecl _ x0 x1 mb0 xs0 mb1 )= Node (toAstNode t) [toAst x0,toAst x1,mb mb0, list xs0,mb mb1]
     
     toAst t@(ClassDecl _ mb0 x0 xs0 mbs0)= Node (toAstNode t) [mb mb0,toAst x0,list xs0, mb' list mbs0]
     toAst t@(InstDecl _ mb0 x0 mb1)= Node (toAstNode t) [mb mb0,toAst x0,mb' list mb1]
     toAst t@(DerivDecl _ mb0 x0 )= Node (toAstNode t) [mb mb0,toAst x0]
     toAst t@(InfixDecl _ x0 mb0 xs0)= Node (toAstNode t) [toAst x0,mb' leaf mb0,list xs0]
     toAst t@(DefaultDecl _ xs0)= Node (toAstNode t) [list xs0]
     toAst t@(SpliceDecl _ x0)= Node (toAstNode t) [toAst x0]
     toAst t@(TypeSig _ xs0 x0)= Node (toAstNode t) [list xs0,toAst x0]
     toAst t@(FunBind _ xs0)= Node (toAstNode t) [list xs0]
     toAst t@(PatBind _ x0 mb0 x1 mb1 )= Node (toAstNode t) [toAst x0,mb mb0,toAst x1,mb mb1]
     toAst t@(ForImp _ x0 mb0 mb1 x1 x2)= Node (toAstNode t) [toAst x0,mb mb0,mb' str mb1,toAst x1,toAst x2]
     toAst t@(ForExp _ x0 mb0 x1 x2)= Node (toAstNode t) [toAst x0,mb' str mb0,toAst x1,toAst x2]
     toAst t@(RulePragmaDecl _ xs0)= Node (toAstNode t) [list xs0]
     toAst t@(DeprPragmaDecl _ xs0)= Node (toAstNode t) [list' (pair' list str) xs0]
     toAst t@(WarnPragmaDecl _ xs0)= Node (toAstNode t) [list' (pair' list str) xs0]
     toAst t@(InlineSig _ b mb0 x0)= Node (toAstNode t) [leaf b,mb mb0,toAst x0]
     toAst t@(InlineConlikeSig _     mb0 x0)= Node (toAstNode t) [mb mb0,toAst x0]
     toAst t@(SpecSig _                             x0 xs0)= Node (toAstNode t) [toAst x0,list xs0]
     toAst t@(SpecInlineSig _ b mb0 x0 xs0)= Node (toAstNode t) [leaf b,mb mb0,toAst x0,list xs0]
     toAst t@(InstSig _      mb0 x0)= Node (toAstNode t) [mb mb0,toAst x0]
     toAst t@(AnnPragma _ x0)= Node (toAstNode t) [toAst x0]


instance ToAst Rule where
  toAst t@(Rule _ s mb0 mblist0 x0 x1) = Node (toAstNode t) [str s,mb mb0,mb' list mblist0,toAst x0, toAst x1]

instance ToAst Annotation where
    toAst t@(Ann _ x0  x1)= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(TypeAnn _ x0  x1)= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(ModuleAnn _           x0)= Node (toAstNode t) [toAst x0]

instance ToAst DeclHead where
    toAst t@(DHead _ x0 xs0)= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(DHInfix _ x0 x1 x2)= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
    toAst t@(DHParen _ x0)= Node (toAstNode t) [toAst x0]

instance ToAst InstHead where
    toAst t@(IHead _ x0 xs0)= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(IHInfix _ x0 x1 x2)= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
    toAst t@(IHParen _ x0)= Node (toAstNode t) [toAst x0]

instance ToAst Binds where
    toAst t@(BDecls _ xs0     )= Node (toAstNode t) [list xs0]
    toAst t@(IPBinds _ xs0   )= Node (toAstNode t) [list xs0]

instance ToAst IPBind where
  toAst t@(IPBind _ x0 x1) = Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst QualConDecl where
    toAst t@(QualConDecl _ mbxs0 mb0 x0)= Node (toAstNode t) [mb' list mbxs0,mb mb0,toAst x0]

instance ToAst ConDecl where
     toAst t@(ConDecl _ x0 xs0)= Node (toAstNode t) [toAst x0,list xs0]
                
     toAst t@(InfixConDecl _ x0 x1 x2)= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
                
     toAst t@(RecDecl _ x0 xs0)= Node (toAstNode t) [toAst x0,list xs0]

instance ToAst FieldDecl where
  toAst t@(FieldDecl _ xs0 x0) = Node (toAstNode t) [list xs0,toAst x0]

instance ToAst GadtDecl where
    toAst t@(GadtDecl _ x0 x1)= Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst ClassDecl where
    toAst t@(ClsDecl _ x0)= Node (toAstNode t) [toAst x0]
    toAst t@(ClsDataFam _ mb0 x0 mb1) = Node (toAstNode t) [mb mb0,toAst x0,mb mb1]
            
    toAst t@(ClsTyFam _  x0 mb0)= Node (toAstNode t) [toAst x0,mb mb0]
    toAst t@(ClsTyDef _ x0 x1)= Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst InstDecl where
    toAst t@(InsDecl _ x0)= Node (toAstNode t) [toAst x0]
    toAst t@(InsType _ x0 x1)= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(InsData _ x0 x1 xs0 mb0)= Node (toAstNode t) [toAst x0,toAst x1,list xs0,mb mb0]
    toAst t@(InsGData _ x0 x1 mb0 xs0 mb1)= Node (toAstNode t) [toAst x0,toAst x1,mb mb0,list xs0,mb mb1]
           

instance ToAst BangType where
     toAst t@(BangedTy _ x0 )= Node (toAstNode t) [toAst x0]
     toAst t@(UnBangedTy _ x0 )= Node (toAstNode t) [toAst x0]
     toAst t@(UnpackedTy _ x0 )= Node (toAstNode t) [toAst x0]

instance ToAst Rhs where
     toAst t@(UnGuardedRhs _ x0 )= Node (toAstNode t) [toAst x0]
     toAst t@(GuardedRhss _ xs0)= Node (toAstNode t) [list xs0]

instance ToAst GuardedRhs where
     toAst t@(GuardedRhs _ xs0 x0)= Node (toAstNode t) [list xs0,toAst x0]

boxed :: Show a => a -> Tree AstNode
boxed x = Node (astnode (show x) Nothing) []

instance ToAst Type where
     toAst t@(TyForall _ mbxs0 mb0 x0)= Node (toAstNode t) [mb' list mbxs0,mb mb0,toAst x0]
     toAst t@(TyFun _ x0 x1              )= Node (toAstNode t) [toAst x0,toAst x1]
     toAst t@(TyTuple _ box xs0          )= Node (toAstNode t) [boxed box,list xs0]
     toAst t@(TyList _ x0                       )= Node (toAstNode t) [toAst x0]
     toAst t@(TyApp _ x0 x1              )= Node (toAstNode t) [toAst x0,toAst x1]
     toAst t@(TyVar _ x0                       )= Node (toAstNode t) [toAst x0]
     toAst t@(TyCon _ x0                      )= Node (toAstNode t) [toAst x0]
     toAst t@(TyParen _ x0                       )= Node (toAstNode t) [toAst x0]
     toAst t@(TyInfix _ x0 x1 x2    )= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
     toAst t@(TyKind _ x0 x1              )= Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst TyVarBind where
    toAst t@(KindedVar _ x0 x1  )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(UnkindedVar _ x0           )= Node (toAstNode t) [toAst x0]

instance ToAst Kind where
    toAst t@(KindStar _                    )= Node (toAstNode t) []
    toAst t@(KindBang _                    )= Node (toAstNode t) []
    toAst t@(KindFn _ x0 x1  )= Node (toAstNode t) [toAst x0, toAst x1]
    toAst t@(KindParen _ x0           )= Node (toAstNode t) [toAst x0]
    toAst t@(KindVar _ x0           )= Node (toAstNode t) [toAst x0]

instance ToAst FunDep where
    toAst t@(FunDep _ xs0 xs1)= Node (toAstNode t) [list xs0,list xs1]

instance ToAst Context where
    toAst t@(CxSingle _ x0)= Node (toAstNode t) [toAst x0]
    toAst t@(CxTuple _ xs0)= Node (toAstNode t) [list xs0]
    toAst t@(CxParen _ x0)= Node (toAstNode t) [toAst x0]
    toAst t@(CxEmpty _)   = Node (toAstNode t) []

instance ToAst Asst where
        toAst t@(ClassA _ x0 xs0           )= Node (toAstNode t) [toAst x0,list xs0]
        toAst t@(InfixA _ x0 x1 x2  )= Node (toAstNode t) [toAst x0,toAst x1,toAst x2] 
        toAst t@(IParam _ x0 x1          )= Node (toAstNode t) [toAst x0,toAst x1]
        toAst t@(EqualP _ x0 x1            )= Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst Literal where
    toAst t@(Char _ c s)= Node (toAstNode t) [leaf c,str s]
    toAst t@(String _ s0 s1)= Node (toAstNode t) [str s0,str s1]
    toAst t@(Int _ i s)= Node (toAstNode t) [leaf i,str s]
    toAst t@(Frac _ r s)= Node (toAstNode t) [leaf r,str s]
    toAst t@(PrimInt _ i s)= Node (toAstNode t) [leaf i,str s]
    toAst t@(PrimWord _ i s)= Node (toAstNode t) [leaf i,str s]
    toAst t@(PrimFloat _ r s)= Node (toAstNode t) [leaf r,str s]
    toAst t@(PrimDouble _ r s)= Node (toAstNode t) [leaf r,str s]
    toAst t@(PrimChar _ c s)= Node (toAstNode t) [leaf c,str s]
    toAst t@(PrimString _ s0 s1)= Node (toAstNode t) [str s0,str s1]

instance ToAst Exp where
    toAst t@(Var _ x0                       )= Node (toAstNode t) [toAst x0]
    toAst t@(IPVar _ x0                    )= Node (toAstNode t) [toAst x0]
    toAst t@(Con _ x0                       )= Node (toAstNode t) [toAst x0]
    toAst t@(Lit _ x0                     )= Node (toAstNode t) [toAst x0]
    toAst t@(InfixApp _ x0 x1 x2    )= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
    toAst t@(App _ x0 x1                 )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(NegApp _ x0                      )= Node (toAstNode t) [toAst x0]
    toAst t@(Lambda _ xs0 x0              )= Node (toAstNode t) [list xs0,toAst x0]
    toAst t@(Let _ x0 x1               )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(If _ x0 x1 x2          )= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
    toAst t@(Case _ x0 xs0                )= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(Do _ xs0                         )= Node (toAstNode t) [list xs0]
    toAst t@(MDo _ xs0                        )= Node (toAstNode t) [list xs0]
    toAst t@(Tuple _ xs0                       )= Node (toAstNode t) [list xs0]
    toAst t@(TupleSection _ xsmb0        )= Node (toAstNode t) [list' mb xsmb0]
    toAst t@(List _ xs0                        )= Node (toAstNode t) [list xs0]
    toAst t@(Paren _ x0                       )= Node (toAstNode t) [toAst x0]
    toAst t@(LeftSection _ x0 x1         )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RightSection _ x0 x1        )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RecConstr _ x0 xs0 )= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(RecUpdate _ x0   xs0 )= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(EnumFrom _ x0                    )= Node (toAstNode t) [toAst x0]
    toAst t@(EnumFromTo _ x0 x1          )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(EnumFromThen _ x0 x1        )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(EnumFromThenTo _ x0 x1 x2)= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
    toAst t@(ListComp _ x0 xs0       )= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(ParComp _ x0 xs0     )= Node (toAstNode t) [toAst x0,list' list xs0]
    toAst t@(ExpTypeSig _ x0 x1         )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(VarQuote _ x0                  )= Node (toAstNode t) [toAst x0]
    toAst t@(TypQuote _ x0                  )= Node (toAstNode t) [toAst x0]
    toAst t@(BracketExp _ x0              )= Node (toAstNode t) [toAst x0]
    toAst t@(SpliceExp _ x0                )= Node (toAstNode t) [toAst x0]
    toAst t@(QuasiQuote _ s0 s1 )= Node (toAstNode t) [str s0,str s1]
    toAst t@(XTag _ x0 xs0 mb0 xs1)= Node (toAstNode t) [toAst x0,list xs0,mb mb0,list xs1]
    toAst t@(XETag _ x0 xs0 mb0)= Node (toAstNode t) [toAst x0,list xs0,mb mb0]
    toAst t@(XPcdata _ s)= Node (toAstNode t) [str s]
    toAst t@(XExpTag _ x0                     )= Node (toAstNode t) [toAst x0]
    toAst t@(XChildTag _ xs0                   )= Node (toAstNode t) [list xs0]
    toAst t@(CorePragma _      s x0      )= Node (toAstNode t) [str s,toAst x0]
    toAst t@(SCCPragma _      s x0      )= Node (toAstNode t) [str s,toAst x0]
    toAst t@(GenPragma _      s p0 p1 x1)= Node (toAstNode t) [str s,pair' leaf leaf p0,pair' leaf leaf p1,toAst x1]
    toAst t@(Proc _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(LeftArrApp _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RightArrApp _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(LeftArrHighApp _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RightArrHighApp _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst XName where
    toAst t@(XName _ s)= Node (toAstNode t) [str s]
    toAst t@(XDomName _ s0 s1)= Node (toAstNode t) [str s0,str s1]

instance ToAst Bracket where
    toAst t@(ExpBracket _ x0        )= Node (toAstNode t) [toAst x0]
    toAst t@(PatBracket _ x0        )= Node (toAstNode t) [toAst x0]
    toAst t@(TypeBracket _ x0      )= Node (toAstNode t) [toAst x0]
    toAst t@(DeclBracket _ xs0      )= Node (toAstNode t) [list xs0]

instance ToAst Splice where
    toAst t@(IdSplice _ s)= Node (toAstNode t) [str s]
    toAst t@(ParenSplice _ x0       )= Node (toAstNode t) [toAst x0]

instance ToAst Safety where
    toAst t@(PlayRisky _         )= Node (toAstNode t) []
    toAst t@(PlaySafe _ b     )= Node (toAstNode t) [leaf b]

instance ToAst CallConv where
    toAst t@(StdCall _ )= Node (toAstNode t) []
    toAst t@(CCall _ )= Node (toAstNode t) []
    toAst t@(CPlusPlus _ )= Node (toAstNode t) []
    toAst t@(DotNet _ )= Node (toAstNode t) []
    toAst t@(Jvm _ )= Node (toAstNode t) []
    toAst t@(Js _ )= Node (toAstNode t) []

instance ToAst ModulePragma where
    toAst t@(LanguagePragma _ xs0  )= Node (toAstNode t) [list xs0]
    toAst t@(OptionsPragma _ mb0 s)= Node (toAstNode t) [mb' tool  mb0,str s]
    toAst t@(AnnModulePragma _ x0)= Node (toAstNode t) [toAst x0]

tool :: Tool -> Tree AstNode
tool (UnknownTool s) = Node (astnode "UnknownTool" Nothing) [str s]
tool GHC = str "GHC" 
tool HUGS = str "HUGS"
tool NHC98 = str "NHC98"
tool YHC = str "YHC"
tool HADDOCK = str "HADDOCK"

instance ToAst Activation where
    toAst t@(ActiveFrom _ i)= Node (toAstNode t) [leaf i]
    toAst t@(ActiveUntil _ i)= Node (toAstNode t) [leaf i]

instance ToAst RuleVar where
    toAst t@(RuleVar _ x0)= Node (toAstNode t) [toAst x0]
    toAst t@(TypedRuleVar _ x0 x1)= Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst WarningText where
    toAst t@(DeprText _ s)= Node (toAstNode t) [str s]
    toAst t@(WarnText _ s)= Node (toAstNode t) [str s]

instance ToAst Pat where
    toAst t@(PVar _ x0                       )= Node (toAstNode t) [toAst x0]
    toAst t@(PLit _ x0                    )= Node (toAstNode t) [toAst x0]
    toAst t@(PNeg _ x0                        )= Node (toAstNode t) [toAst x0]
    toAst t@(PNPlusK _ x0 i)= Node (toAstNode t) [toAst x0,leaf i]
    toAst t@(PInfixApp _ x0 x1 x2 )= Node (toAstNode t) [toAst x0,toAst x1,toAst x2]
    toAst t@(PApp _ x0 xs0              )= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(PTuple _ xs0                      )= Node (toAstNode t) [list xs0]
    toAst t@(PList _ xs0                       )= Node (toAstNode t) [list xs0]
    toAst t@(PParen _ x0                      )= Node (toAstNode t) [toAst x0]
    toAst t@(PRec _ x0 xs0         )= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(PAsPat _ x0 x1             )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(PWildCard _                           )= Node (toAstNode t) []
    toAst t@(PIrrPat _ x0                     )= Node (toAstNode t) [toAst x0]
    toAst t@(PatTypeSig _ x0 x1         )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(PViewPat _ x0 x1            )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(PRPat _ xs0                      )= Node (toAstNode t) [list xs0]
    toAst t@(PXTag _ x0 xs0 mb0 xs1)= Node (toAstNode t) [toAst x0,list xs0,mb mb0,list xs1]
    toAst t@(PXETag _ x0 xs0 mb0)= Node (toAstNode t) [toAst x0,list xs0,mb mb0]
    toAst t@(PXPcdata _ s)= Node (toAstNode t) [str s]
    toAst t@(PXPatTag _ x0                    )= Node (toAstNode t) [toAst x0]
    toAst t@(PXRPats _ xs0                   )= Node (toAstNode t) [list xs0]
    toAst t@(PExplTypeArg _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(PQuasiQuote _ s1 s2)= Node (toAstNode t) [str s1,str s2]
    toAst t@(PBangPat _ x0                    )= Node (toAstNode t) [toAst x0]

leaf :: Show a => a -> Tree AstNode
leaf i = Node (astnode (show i) Nothing) []

str :: String -> Tree AstNode
str s = Node (astnode s Nothing) []

instance ToAst PXAttr where
  toAst t@(PXAttr _ x0 x1) = Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst RPatOp where
    toAst t@(RPStar _  )= Node (toAstNode t) []
    toAst t@(RPStarG _  )= Node (toAstNode t) []
    toAst t@(RPPlus _  )= Node (toAstNode t) []
    toAst t@(RPPlusG _  )= Node (toAstNode t) []
    toAst t@(RPOpt _  )= Node (toAstNode t) []
    toAst t@(RPOptG _  )= Node (toAstNode t) []

instance ToAst RPat where
    toAst t@(RPOp _ x0 x1   )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RPEither _ x0 x1 )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RPSeq _ xs0             )= Node (toAstNode t) [list xs0]
    toAst t@(RPGuard _ x0 xs0   )= Node (toAstNode t) [toAst x0,list xs0]
    toAst t@(RPCAs _ x0 x1    )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RPAs _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(RPParen _ x0           )= Node (toAstNode t) [toAst x0]
    toAst t@(RPPat _ x0              )= Node (toAstNode t) [toAst x0]

instance ToAst PatField where
    toAst t@(PFieldPat _ x0 x1     )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(PFieldPun _ x0              )= Node (toAstNode t) [toAst x0]
    toAst t@(PFieldWildcard _                  )= Node (toAstNode t) []

instance ToAst Stmt where
    toAst t@(Generator _ x0 x1)= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(Qualifier _ x0   )= Node (toAstNode t) [toAst x0]
    toAst t@(LetStmt _ x0   )= Node (toAstNode t) [toAst x0]
    toAst t@(RecStmt _ xs0    )= Node (toAstNode t) [list xs0]

instance ToAst QualStmt where
    toAst t@(QualStmt _ x0         )= Node (toAstNode t) [toAst x0]
    toAst t@(ThenTrans _ x0          )= Node (toAstNode t) [toAst x0]
    toAst t@(ThenBy _ x0 x1  )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(GroupBy _ x0          )= Node (toAstNode t) [toAst x0]
    toAst t@(GroupUsing _ x0          )= Node (toAstNode t) [toAst x0]
    toAst t@(GroupByUsing _ x0 x1  )= Node (toAstNode t) [toAst x0,toAst x1]

instance ToAst FieldUpdate where
    toAst t@(FieldUpdate _ x0 x1    )= Node (toAstNode t) [toAst x0,toAst x1]
    toAst t@(FieldPun _ x0                )= Node (toAstNode t) [toAst x0]
    toAst t@(FieldWildcard _                    )= Node (toAstNode t) []

instance ToAst Alt where
    toAst t@(Alt _ x0 x1 mb0)= Node (toAstNode t) [toAst x0,toAst x1,mb mb0]

instance ToAst GuardedAlts where
    toAst t@(UnGuardedAlt _ x0         )= Node (toAstNode t) [toAst x0]
    toAst t@(GuardedAlts _ xs0  )= Node (toAstNode t) [list xs0]

instance ToAst GuardedAlt where
    toAst t@(GuardedAlt _ xs0 x0)= Node (toAstNode t) [list xs0,toAst x0]

