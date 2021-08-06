module Ohua.Compat.Clike.Types where


import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc (Pretty, pretty, (<+>))
import Data.Functor.Foldable (para)
import qualified Prelude as P

import Ohua.Prelude
import Ohua.Frontend.Lang
import Ohua.Types
import Ohua.Frontend.NS
import Ohua.ALang.PPrint (quickRender)
import Ohua.ALang.PPrint ()

data Decl
    = ImportDecl Import
    | AlgoDecl Algo
    | PragmaD Pragma
    | MacroCall Expr

data Import = Import
    { isAlgo :: Bool
    , nsRef :: NSRef
    , bindings :: [Binding]
    }
data Algo = Algo
    { algoName :: Binding
    , algoTyAnn :: FunAnn RustTyExpr
    , algoCode :: Expr
    } deriving ( Show, Eq )


type Mutability = Bool

pattern Mutable :: value -> Annotated Bool value
pattern Mutable v = Annotated True v
pattern Immutable :: value -> Annotated Bool value
pattern Immutable v = Annotated False v

type RustTyExpr = Annotated Mutability (TyExpr SomeBinding)

tupleConstructor :: TyExpr SomeBinding
tupleConstructor = TyRef $ Qual $ QualifiedBinding [] "(,)"

instance Pretty Decl where
    pretty = \case
        ImportDecl i -> "Import Declaration" <+> PP.squotes (pretty i)
        AlgoDecl a -> "Algo Declaraion" <+> PP.squotes (pretty a)
        PragmaD p -> "Pragma" <+> PP.squotes (pretty p)
        MacroCall m -> "Macro Call" <+> PP.squotes (pretty m)

instance Pretty Algo where
    pretty Algo{..} = PP.hsep
        [ pretty algoName
        , pretty algoTyAnn
        , PP.braces (PP.line <> PP.nest 4 (pretty algoCode) <> PP.line)
        ]

instance Pretty (FunAnn RustTyExpr) where
    pretty FunAnn{..} =
        PP.tupled (map pretty argTypes) <+> "->" <+> pretty retType

instance Pretty RustTyExpr where
    pretty = \case
        Mutable v -> "mut" <+> pval v
        Immutable v -> pval v
      where
        pval v = pretty base <> if null targs then mempty else PP.encloseSep "<" ">" "," (map pval targs)
          where
            (base, targs) = unravelTyExp v

unravelTyExp = para $ \case
    TyRefF r -> (r, [])
    TyAppF (_, (b, args) ) (arg, _) -> (b, args ++ [arg])

prettyLit = \case
    UnitLit -> "()"
    NumericLit n -> pretty n
    EnvRefLit e -> "env" <> PP.braces ( pretty e )
    FunRefLit (FunRef r _)
        | r == "ohua.lang/&" -> "&"
        | otherwise -> PP.encloseSep mempty mempty "::" $ map pretty (unwrap (r ^. namespace) ++ [ r ^. name ])

instance Pretty Expr where
    pretty = \case
        VarE b -> pretty b
        LitE l -> prettyLit l
        LetE pat e1 e2 -> PP.hsep ["let", pretty pat, "=", pretty e1] <> ";" <> PP.line <> pretty e2
        AppE f args ->
            case f of
                LitE (FunRefLit (FunRef (QualifiedBinding ["ohua", "lang"] fun) _))
                    | fun == "&" -> "&" <> (pretty $ P.head args)
                    | fun `elem` ( [ "==", "<=", "<", ">", ">=", "||", "&&" , "!="] :: Vector Binding ) ->
                      let [a1, a2] = args in
                          PP.parens $ pretty a1 <+> pretty fun <+> pretty a2
                _ -> pretty f <> PP.tupled (map pretty args)
        LamE args body -> PP.encloseSep "|" "|" ", " (map pretty args) <+> PP.braces (PP.softline <> PP.nest 4 (pretty body) <> PP.softline)
        IfE cond thn els -> PP.hsep ["if", pretty cond, PP.braces (PP.pretty thn), "else", PP.braces (pretty els)]
        MapE fn coll
             | LamE [p] bd <- fn -> PP.hsep ["for", pretty p, "in", pretty coll, PP.braces (PP.line <> PP.nest 4 (pretty bd) <> PP.line)]
             | otherwise -> pretty $ AppE (VarE "map") [fn, coll]
        BindE st fn -> pretty st <> PP.dot <> pretty fn
        StmtE stmt cont -> pretty stmt <> ";" <> PP.hardline <> pretty cont
        SeqE _ _ -> "<seq expr>"
        TupE things -> PP.tupled (map pretty things)

instance Pretty Pat where
    pretty = \case
        VarP b -> pretty b
        TupP pats -> PP.tupled (map pretty pats)
        UnitP -> "()"

instance Pretty Import where
    pretty Import {..} = PP.hsep
        [ "use"
        , if isAlgo then "also" else "sf"
        , pretty nsRef
        , PP.encloseSep PP.lbrace PP.rbrace ", " (map pretty bindings)
        ]

instance Pretty Pragma where
    pretty = \case
        Feature f -> "//#feature" <+> pretty f
        Other p v -> "//#" <> pretty p <+> pretty v

instance Pretty (Annotated (FunAnn RustTyExpr ) Expr) where
    pretty (Annotated ann val) = pretty ann <> PP.line <> pretty val
