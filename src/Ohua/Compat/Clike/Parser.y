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

import Ohua.Compat.Clike.Lexer
import Ohua.Compat.Clike.Types
import Ohua.ALang.Lang
import Ohua.Types
import Ohua.ALang.NS
import qualified Data.HashMap.Strict as HM
import Data.Either
import Data.Maybe
import qualified Ohua.ParseTools.Refs as Refs

}


%name parseExpH Exp
%name parseNSRaw NS
%name parseTLFunDef TLFunDef
%tokentype { Lexeme }
%error { parseError }

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
    : id     { [$1] }
    | qualid { $1 }

SomeId 
    : id     { Unqual $1 }
    | qualid { Qual (toQualBnd $1) }

Stmts
    : NoSemStmt Stmts   { $1 $2 }
    | StmtSem ';' Stmts { $1 $3 }
    | Exp               { $1 }

NoSemStmt
    : NamedFunDef           { let (name, fun) = $1 in Let (Direct name) fun }

StmtSem
    : Exp                   { ignoreArgLet $1 }
    | let Destruct '=' Exp  { Let $2 $4 }

Exp 
    : fn FunDef                             { $2 }
    | SomeId '(' Apply ')'                  { $3 (Var $1) }
    | SomeId                                { Var $1 }
    | for Destruct in Exp '{' Stmts '}'     { Refs.smapBuiltin `Apply` Lambda $2 $6 `Apply` $4 }
    | if '(' Exp ')'
           '{' Stmts '}'
      else '{' Stmts '}'                    { Refs.ifBuiltin `Apply` $3 `Apply` ignoreArgLambda $6 `Apply` ignoreArgLambda $10 }
    | '{' Stmts '}'                         { $2 }

Destruct 
    : id           { Direct $1 }
    | '(' Vars ')' { Destructure $2 }

Vars 
    : id ',' Vars   { $1 : $3 }
    | id            { [$1] }

Apply
    : ApplyParams   { $1 }
    |               { id }

ApplyParams
    : Exp ',' Apply  { $3 . (`Apply` $1) }
    | Exp            { (`Apply` $1) }

FunDef 
    : '(' Params ')' FunBody { $2 $4 }

FunBody
    : '{' Stmts '}' { $2 }

Params
    : HasParams { $1 }
    |           { ignoreArgLambda }

HasParams
    : Destruct ',' HasParams   { Lambda $1 . $3 }
    | Destruct                 { Lambda $1 }

NamedFunDef
    : fn id FunDef { ($2, $3) }

TLFunDef
    : fn id '(' AnnParams ')' FunRetAnn FunBody { let (types, e) = $4 in ($2, Annotated (FunAnn types $ Immutable $6) (e $7)) }

AnnParams
    : HasAnnParams { $1 }
    |              { ([], ignoreArgLambda) }

HasAnnParams
    : Destruct ':' InputTyAnn ',' HasAnnParams { let (l, e) = $5 in ($3 : l, Lambda $1 . e) }
    | Destruct ':' InputTyAnn                  { ([$3], Lambda $1) }

InputTyAnn
    : mut TyExpr { Mutable $2 }
    | TyExpr     { Immutable $1 } 

FunRetAnn
    : '->' TyExpr { $2 }
    |             { tupleConstructor }

TyExpr
    : '(' TyExprLst ')'           { foldl TyApp tupleConstructor $2 }
    | TyExpr '<' TyExprLst '>' { foldl TyApp $1 ($3 :: [TyExpr SomeBinding]) }
    | SomeId                   { TyRef $1 }

TyExprLst
    : NonEmptyTyExprList { $1 }
    |                    { [] }

NonEmptyTyExprList
    : TyExpr ',' NonEmptyTyExprList  { $1 : $3 }
    | TyExpr                         { [$1] :: [TyExpr SomeBinding] }

NS 
    : ns QualId ';' Defs { mkNS (bndsToNSRef $2) $4 }

Defs 
    : Def Defs  { $1 : $2 }
    | Def       { [$1] }

Def : use sf Reqdefs ';'    { Left (Left $3) }
    | use algo Reqdefs ';'  { Left (Right $3) }
    | TLFunDef              { Right $1 }

Reqdefs 
    : ReqDef ',' Reqdefs    { $1 : $3 }
    | ReqDef                { [$1] }

ReqDef 
    : QualId '(' Refers ')' { (bndsToNSRef $1, $3) }
    | QualId                { (bndsToNSRef $1, []) }

Refers 
    : id ',' Refers    { $1 : $3 }
    | id               { [$1] }
    |                  { [] }

{


ignoreArgLambda :: Expr SomeBinding -> Expr SomeBinding
ignoreArgLambda = Lambda (Direct "_")
ignoreArgLet :: Expr SomeBinding -> Expr SomeBinding -> Expr SomeBinding
ignoreArgLet = Let (Direct "_")

bndsToNSRef :: [Binding] -> NSRef
bndsToNSRef = nsRefFromList


toQualBnd :: [Binding] -> QualifiedBinding
toQualBnd [] = error "empty id"
toQualBnd [x] = error "qual bnd with only one component"
toQualBnd xs = QualifiedBinding (nsRefFromList $ init xs) (last xs)


-- | Parse a stream of tokens into a simple ALang expression
parseExp :: [Lexeme] -> Expr SomeBinding
parseExp = parseExpH

parseError :: [Lexeme] -> a
parseError tokens = error $ "Parse error" ++ show tokens


-- | Parse a stream of tokens into a namespace
parseNS :: [Lexeme] -> Namespace (Annotated (FunAnn RustTyExpr) (Expr SomeBinding))
parseNS = parseNSRaw

mkNS :: NSRef -> [Either (Either [(NSRef, [Binding])] [(NSRef, [Binding])]) (Binding, Annotated (FunAnn RustTyExpr) (Expr SomeBinding))] -> Namespace (Annotated (FunAnn RustTyExpr) (Expr SomeBinding))
mkNS name defs = Namespace name (concat algoRequires) (concat sfRequires) fundefs
  where
    (requireList, fundefList) = partitionEithers defs
    (sfRequires, algoRequires) = partitionEithers requireList
    fundefs = HM.fromList fundefList

}
