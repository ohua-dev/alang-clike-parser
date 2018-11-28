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
    :: { SomeBinding }
    : QualId { case $1 of x:|[] -> VarE x; xs -> LitE $ FunRefLit $ FunRef (toQualBnd $1) Nothing }

Stmts
    :: { RawExpression }
    : many_sep(StmtSem, ';') opt(Exp)   { foldl (.) $1 id (fromMaybe (Lit UnitLit) $2) }

Block
    :: { Expr }
    : '{' Stmts '}' { $2 }

-- Statments that do require semicolons
StmtSem
    :: { RawExpressionProducer }
    : Exp                   { StmtE $1 }
    | let Pat '=' Exp       { LetE $2 $4 }

SimpleExp :: { RawExpression }
    : '(' many_sep(Exp, ',') ')' { case $2 of
                                       [] -> LitE UnitLit
                                       [x] -> x
                                       xs -> TupE xs }
    | Block                                 { $1 }
    | SomeId                                { $1 }

Exp
    :: { RawExpression }
    : '|' many_sep(Pat, ',') '|' SimpleExp  { LamE $2 $4 }
    | SimpleExp '(' many_sep(Exp, ',') ')'  { $1 `AppE` $3 }
    | for Pat in Exp Block                  { MapE (LamE [$2] $5) $4 }
    | if SimpleExp
           Block
      else Block                            { IfE $2 $3 $5 }
    | Exp with Exp                          { BindE $1 $3 }
    | SimpleExp                             { $1 }

Pat
    :: { Pat }
    : id               { VarP $1 }
    | '(' many_sep(Pat, ',') ')' { case $2 of [] -> UnitP; [x] -> x; xs -> TupP xs }

TLFunDef
    :: { (Binding, Annotated (FunAnn RustTyExpr) RawExpression) }
    : fn id '(' AnnParams ')' FunRetAnn Block { let (types, e) = $4 in ($2, Annotated (FunAnn types $ Immutable $6) (e $7)) }

AnnParams
    :: { ([RustTyExpr], RawExpressionProducer) }
    : many_sep(AnnPat, ',') { second LamE $ unzip $1 }

AnnPat
    : Pat ':' InputTyAnn  { ($1, $3) }

InputTyAnn
    :: { RustTyExpr }
    : mut TyExpr { Mutable $2 }
    | TyExpr     { Immutable $1 }

FunRetAnn
    :: { TyExpr SomeBinding }
    : '->' TyExpr { $2 }
    |             { tupleConstructor }

TyExprList
    :: { [TyExpr SomeBinding] }
    : many_sep(TyExpr, ',') { $1 }

TyExpr
    :: { TyExpr SomeBinding }
    : '(' TyExprList ')'           { foldl TyApp tupleConstructor $2 }
    | TyExpr '<' TyExprList '>'   { foldl TyApp $1 ($3 :: [TyExpr SomeBinding]) }
    | SomeId                      { TyRef $1 }

NS  :: { Namespace (Annotated (FunAnn RustTyExpr) Expr) }
    : ns QualId ';' Defs { mkNS (bndsToNSRef $2) $4 }

Defs
    :: { [Def] }
    : Def Defs  { $1 : $2 }
    | Def       { [$1] }

Def :: { Def }
    : use ReqDef ';'        { Left $2 }
    | TLFunDef              { Right $1 }

ReqDef
    :: { ReqDef }
    : ReqType QualId opt(Refers) '(' Refers ')' { ($1, bndsToNSRef $2, fromMaybe [] $3) }

ReqType
    :: { Bool }
    : algo { True }
    | sf   { False }

Refers
    :: { [Binding] }
    : '(' many_sep(id, ',') ')' { $2 }

{

type Input = BS.ByteString

type PM = Alex

type RawExpression = Expr

type RawExpressionProducer = RawExpression -> RawExpression

type ReqDef = (Bool, NSRef, [Binding])

type FunDef = ( Binding
              , Annotated (FunAnn RustTyExpr) Expr)

type Def = Either (Either [ReqDef] [ReqDef]) FunDef


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

parseTLFunDef :: Input -> FunDef
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

mkNS :: NSRef -> [Def] -> Namespace (Annotated (FunAnn RustTyExpr) Expr)
mkNS name defs = (emptyNamespace name :: Namespace ())
     & algoImports .~ concat algoRequires
     & sfImports .~ concat sfRequires
     & decls .~ fundefs
  where
    (requireList, fundefList) = partitionEithers defs
    (sfRequires, algoRequires) = partitionEithers requireList
    fundefs = HM.fromList fundefList

}
