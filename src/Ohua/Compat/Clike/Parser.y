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
import Ohua.ALang.Lang
import Ohua.Types
import Ohua.ALang.NS
import Ohua.Unit
import qualified Data.HashMap.Strict as HM
import qualified Ohua.ParseTools.Refs as Refs
import Ohua.ALang.Refs (mkTuple)
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

    id      { UnqualId $$ }
    qualid  { QualId $$ }

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
    ','     { Comma }
    '('     { LParen }
    ')'     { RParen }
    '{'     { LBrace }
    '}'     { RBrace }
    ':'     { Colon }
    ';'     { Semicolon }
    '<'     { LAngle }
    '>'     { RAngle }
    '->'    { RArrow }

%%

QualId
    :: { [Binding] }
    : id     { [$1] }
    | qualid { $1 }

SomeId
    :: { SomeBinding }
    : id     { Unqual $1 }
    | qualid { Qual (toQualBnd $1) }

Stmts
    :: { RawExpression }
    : NoSemStmt Stmts   { $1 $2 }
    | StmtSem ';' Stmts { $1 $3 }
    | Exp               { $1 }

-- Statements that do not require remicolons
NoSemStmt
    :: { RawExpressionProducer }
    : NamedFunDef           { let (name, fun) = $1 in Let (Direct name) fun }

-- Statments that do require semicolons
StmtSem
    :: { RawExpressionProducer }
    : Exp                   { ignoreArgLet $1 }
    | let Destruct '=' Exp  { Let $2 $4 }

Exp
    :: { RawExpression }
    : fn FunDef                             { $2 }
    | SomeId '(' Apply ')'                  { $3 (Var $1) }
    | SomeId                                { Var $1 }
    | for Destruct in Exp '{' Stmts '}'     { Refs.smapBuiltin `Apply` Lambda $2 $6 `Apply` $4 }
    | if '(' Exp ')'
           '{' Stmts '}'
      else '{' Stmts '}'                    { Refs.ifBuiltin `Apply` $3 `Apply` ignoreArgLambda $6 `Apply` ignoreArgLambda $10 }
    | '{' Stmts '}'                         { $2 }
    | '(' Apply  ')'                        { $2 (Var $ Qual mkTuple) }

Destruct
    :: { Assignment }
    : id           { Direct $1 }
    | '(' Vars ')' { Destructure $2 }

Vars
    :: { [Binding] }
    : id ',' Vars   { $1 : $3 }
    | id            { [$1] }

Apply
    :: { RawExpressionProducer }
    : ApplyParams   { $1 }
    |               { (`Apply` someUnitExpr) }

ApplyParams
    :: { RawExpressionProducer }
    : Exp ',' Apply  { $3 . (`Apply` $1) }
    | Exp            { (`Apply` $1) }

FunDef
    :: { RawExpression }
    : '(' Params ')' FunBody { $2 $4 }

FunBody
    :: { RawExpression }
    : '{' Stmts '}' { $2 }

Params
    :: { RawExpressionProducer }
    : HasParams { $1 }
    |           { ignoreArgLambda }

HasParams
    :: { RawExpressionProducer }
    : Destruct ',' HasParams   { Lambda $1 . $3 }
    | Destruct                 { Lambda $1 }

NamedFunDef
    :: { (Binding, RawExpression) }
    : fn id FunDef { ($2, $3) }

TLFunDef
    :: { (Binding, Annotated (FunAnn RustTyExpr) RawExpression) }
    : fn id '(' AnnParams ')' FunRetAnn FunBody { let (types, e) = $4 in ($2, Annotated (FunAnn types $ Immutable $6) (e $7)) }

AnnParams
    :: { ([RustTyExpr], RawExpressionProducer) }
    : HasAnnParams { $1 }
    |              { ([], ignoreArgLambda) }

HasAnnParams
    :: { ([RustTyExpr], RawExpressionProducer) }
    : Destruct ':' InputTyAnn ',' HasAnnParams { let (l, e) = $5 in ($3 : l, Lambda $1 . e) }
    | Destruct ':' InputTyAnn                  { ([$3], Lambda $1) }

InputTyAnn
    :: { RustTyExpr }
    : mut TyExpr { Mutable $2 }
    | TyExpr     { Immutable $1 }

FunRetAnn
    :: { TyExpr SomeBinding }
    : '->' TyExpr { $2 }
    |             { tupleConstructor }

TyExpr
    :: { TyExpr SomeBinding }
    : '(' TyExprLst ')'           { foldl TyApp tupleConstructor $2 }
    | TyExpr '<' TyExprLst '>' { foldl TyApp $1 ($3 :: [TyExpr SomeBinding]) }
    | SomeId                   { TyRef $1 }

TyExprLst
    :: { [TyExpr SomeBinding] }
    : NonEmptyTyExprList { $1 }
    |                    { [] }

NonEmptyTyExprList
    :: { [TyExpr SomeBinding] }
    : TyExpr ',' NonEmptyTyExprList  { $1 : $3 }
    | TyExpr                         { [$1] :: [TyExpr SomeBinding] }

NS
    :: { Namespace (Annotated (FunAnn RustTyExpr) (Expr SomeBinding)) }
    : ns QualId ';' Defs { mkNS (bndsToNSRef $2) $4 }

Defs
    :: { [Def] }
    : Def Defs  { $1 : $2 }
    | Def       { [$1] }

Def :: { Def }
    : use sf Reqdefs ';'    { Left (Left $3) }
    | use algo Reqdefs ';'  { Left (Right $3) }
    | TLFunDef              { Right $1 }

Reqdefs
    :: { [ReqDef] }
    : ReqDef ',' Reqdefs    { $1 : $3 }
    | ReqDef                { [$1] }

ReqDef
    :: { ReqDef }
    : QualId '(' Refers ')' { (bndsToNSRef $1, $3) }
    | QualId                { (bndsToNSRef $1, []) }

Refers
    :: { [Binding] }
    : id ',' Refers    { $1 : $3 }
    | id               { [$1] }
    |                  { [] }

{

type Input = BS.ByteString

type PM = Alex

type RawExpression = Expr SomeBinding

type RawExpressionProducer = RawExpression -> RawExpression

type ReqDef = (NSRef, [Binding])

type FunDef = ( Binding
              , Annotated (FunAnn RustTyExpr) (Expr SomeBinding))

type Def = Either (Either [ReqDef] [ReqDef]) FunDef


ignoreArgLambda :: Expr SomeBinding -> Expr SomeBinding
ignoreArgLambda = Lambda (Direct "_")
ignoreArgLet :: Expr SomeBinding -> Expr SomeBinding -> Expr SomeBinding
ignoreArgLet = Let (Direct "_")

bndsToNSRef :: [Binding] -> NSRef
bndsToNSRef = makeThrow


toQualBnd :: [Binding] -> QualifiedBinding
toQualBnd [] = error "empty id"
toQualBnd [x] = error "qual bnd with only one component"
toQualBnd (x:xs) = QualifiedBinding (bndsToNSRef $ init safeList) (last safeList)
  where
    safeList = x :| xs

runPM :: PM a -> Input -> a
runPM ac bs = either (error . toText) identity $ runAlex bs ac

lexer :: (Lexeme -> PM a) -> PM a
lexer cont = nextToken >>= cont

nextToken :: PM Lexeme
nextToken = alexMonadScan



-- | Parse a stream of tokens into a simple ALang expression
parseExp :: Input -> Expr SomeBinding
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
parseNS :: Input -> Namespace (Annotated (FunAnn RustTyExpr) (Expr SomeBinding))
parseNS = runPM parseNSRawM

mkNS :: NSRef -> [Def] -> Namespace (Annotated (FunAnn RustTyExpr) (Expr SomeBinding))
mkNS name defs = (emptyNamespace name :: Namespace ())
     & algoImports .~ concat algoRequires
     & sfImports .~ concat sfRequires
     & decls .~ fundefs
  where
    (requireList, fundefList) = partitionEithers defs
    (sfRequires, algoRequires) = partitionEithers requireList
    fundefs = HM.fromList fundefList

}
