{
-- |
-- Module      : $Header$
-- Description : Parser for ALang S-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Ohua.Compat.Clike.Parser
    ( parseNS, parseExp, parseTLFunDef
    , Namespace(..)
    , Algo(..)
    ) where

import Ohua.Prelude

import Ohua.Compat.Clike.Lexer
import Ohua.Compat.Clike.Types
import Ohua.Frontend.Lang
import Ohua.Types
import Ohua.Frontend.NS
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BS

--import Unsafe
import Prelude ((!!))
}


%name parseExpM Exp
%name parseNSRawM NS
%name parseTLFunDefM TLFunDef
%name parseTyAnnM InputTyAnn
%tokentype { Lexeme }
%error { parseError }
%lexer { lexer } { EOF }
%monad { PM }

%token

    id      { Id $$ }
    number  { Number $$ }

    '='     { OpEq }
    fn      { KWFn }
    use     { KWUse }
    algo    { KWAlgo }
    sf      { KWSf }
    ns      { KWNS }
    let     { KWLet }
    if      { KWIf }
    else    { KWElse }
    for     { KWFor }
    in      { KWIn }
    mut     { KWMut }
    with    { KWWith }
    ','     { Comma }
    '('     { LParen }
    ')'     { RParen }
    '{'     { LBrace }
    '}'     { RBrace }
    '::'    { DoubleColon }
    ':'     { Colon }
    ';'     { Semicolon }
    '<'     { LAngle }
    '>'     { RAngle }
    '->'    { RArrow }
    '|'     { Pipe }

%%

many1 (p)
    : p many(p) { $1 :| $2 }

many (p)
    : p many(p)  { $1 : $2 }
    |            { [] }

many_sep1(p, sep)
    : p sep many_sep1(p, sep) { let x :| xs = $3 in $1 :| x:xs }
    | p                       { $1 :| [] }

many_sep(p, sep)
    : many_sep1(p, sep) { toList $1 }
    |                   { [] }

braces(p)
    : '{' p '}' { $2 }

parens(p)
    : '(' p ')' { $2 }

opt(p)
    : p { Just $1 }
    |   { Nothing }

or(a, b)
    : a { Left $1 }
    | b { Right $1 }

QualId
    :: { NonEmpty Binding }
    : many_sep1(id, '::') { $1 }

SomeId
    :: { Expr }
    : QualId { case $1 of
                   x:|[] -> VarE x
                   xs -> LitE $ FunRefLit $ FunRef (toQualBnd $1) Nothing }

-- The following three rules are the complicated machinery that enables void
-- functions. This handles the termination cases for blocks. This can either be
-- an expression (to be returned), or nothing, in which case we return unit

Stmts
    : let Pat '=' Exp ';' Stmts { LetE $2 $4 $6 }
    | Exp ContStmts0 { $2 $1 }

ContStmts0
    : ';' ContStmts1 { flip StmtE $2 }
    |                { id }

ContStmts1
    : Stmts { $1 }
    |       { LitE UnitLit }

Block
    :: { Expr }
    : braces(Stmts) { $1 }

StmtSem
    :: { Either Expr (Pat, Expr) }
    : Exp                   { Left $1 }
    | let Pat '=' Exp       { Right ($2, $4) }

SimpleExp :: { RawExpression }
    : parens(many_sep(Exp, ',')) { case $1 of
                                       [] -> LitE UnitLit
                                       [x] -> x
                                       xs -> TupE xs }
    | Block                                 { $1 }
    | SomeId                                { $1 }
    | number                                { LitE (NumericLit $1) }

Exp
    :: { RawExpression }
    : '|' many_sep(Pat, ',') '|' SimpleExp  { LamE $2 $4 }
    | SimpleExp parens(many_sep(Exp, ','))  { $1 `AppE` $2 }
    | for Pat in Exp Block                  { MapE (LamE [$2] $5) $4 }
    | if SimpleExp
           Block
      else Block                            { IfE $2 $3 $5 }
    | Exp with Exp                          { BindE $1 $3 }
    | SimpleExp                             { $1 }

Pat
    :: { Pat }
    : id                         { VarP $1 }
    | parens(many_sep(Pat, ',')) { case $1 of
                                       [] -> UnitP
                                       [x] -> x
                                       xs -> TupP xs }

TLFunDef
    :: { Algo }
    : fn id parens(many_sep(AnnPat, ',')) FunRetAnn Block
        { let (pats, types) = unzip $3
          in Algo { algoName = $2
                  , algoTyAnn = FunAnn types $ Immutable $4
                  , algoCode = LamE pats $5
                  } }

AnnPat
    : Pat ':' InputTyAnn  { ($1, $3) }

InputTyAnn
    :: { RustTyExpr }
    : opt(mut) TyExpr { maybe Immutable (const Mutable) $1 $2 }

FunRetAnn
    :: { TyExpr SomeBinding }
    : '->' TyExpr { $2 }
    |             { tupleConstructor }

TyExprList
    :: { [TyExpr SomeBinding] }
    : many_sep(TyExpr, ',') { $1 }

TyExpr
    :: { TyExpr SomeBinding }
    : parens(TyExprList)          { foldl TyApp tupleConstructor $1 }
    | TyExpr '<' TyExprList '>'   { foldl TyApp $1 ($3 :: [TyExpr SomeBinding]) }
    | or(QualId, id)              { TyRef $ either (Qual . toQualBnd) Unqual $1 }

NS  :: { Namespace (Annotated (FunAnn RustTyExpr) Expr) }
    : ns QualId ';' many(Decl) { mkNS (bndsToNSRef $2) $4 }

Decl :: { Decl }
    : use ReqDef ';'        { ImportDecl $2 }
    | TLFunDef              { AlgoDecl $1 }

Import
    : id ImportCont { $2 $1 }
    | braces(many_sep(id, ',')) { ([], $1) }

ImportCont
    : '::' Import { \x -> first (x:) $2 }
    |             { \x -> ([], [x]) }

ReqDef
    :: { Import }
    : ReqType Import
        { Import { isAlgo = $1
                 , nsRef = bndsToNSRef $ fst $2
                 , bindings = snd $2
                 } }

ReqType
    :: { Bool }
    : algo { True }
    | sf   { False }

{

type Input = BS.ByteString

type PM = Alex

type RawExpression = Expr

type RawExpressionProducer = RawExpression -> RawExpression

data Decl
    = ImportDecl Import
    | AlgoDecl Algo
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


bndsToNSRef :: (Container c, Element c ~ Binding) => c -> NSRef
bndsToNSRef = makeThrow . toList


toQualBnd :: NonEmpty Binding -> QualifiedBinding
toQualBnd [x] = error "qual bnd with only one component"
toQualBnd xs = QualifiedBinding (bndsToNSRef $ init xs) (last xs)

runPM :: PM a -> Input -> a
runPM ac bs = either (error . toText) identity $ runAlex bs ac

lexer :: (Lexeme -> PM a) -> PM a
lexer cont = nextToken >>= cont

nextToken :: PM Lexeme
nextToken = alexMonadScan



-- | Parse a stream of tokens into a simple ALang expression
parseExp :: Input -> Expr
parseExp = runPM parseExpM

parseTLFunDef :: Input -> Algo
parseTLFunDef = runPM parseTLFunDefM

parseTyAnn :: Input -> RustTyExpr
parseTyAnn = runPM parseTyAnnM

parseError :: Lexeme -> PM a
parseError token = do
  (line, col) <- getLexerPos
  throwError $ "Parse error at line " <> show line <> ", column " <> show col <> ", on token " <> show token


-- | Parse a stream of tokens into a namespace
parseNS :: Input -> Namespace (Annotated (FunAnn RustTyExpr) Expr)
parseNS = runPM parseNSRawM

mkNS :: NSRef -> [Decl] -> Namespace (Annotated (FunAnn RustTyExpr) Expr)
mkNS name defs = (emptyNamespace name :: Namespace ())
     & algoImports .~ map importToTuple algoRequires
     & sfImports .~ map importToTuple sfRequires
     & decls .~ algos
  where
    (requires, algoList) = partitionDecls defs
    sfRequires = filter (not . isAlgo) requires
    algoRequires = filter isAlgo requires
    importToTuple Import{nsRef, bindings} = (nsRef, bindings)
    algos = HM.fromList $ map (\Algo{algoTyAnn, algoName, algoCode} -> (algoName, Annotated algoTyAnn algoCode)) algoList -- ignores algos which are defined twice

partitionDecls :: [Decl] -> ([Import], [Algo])
partitionDecls = flip foldr' ([],[]) . flip $ \(xs,ys) -> \case
    ImportDecl i -> (i:xs, ys)
    AlgoDecl a -> (xs, a:ys)

}
