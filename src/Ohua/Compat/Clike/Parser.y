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
    ( parseNS, parseExp
    , Namespace(..)
    ) where

import Ohua.Compat.Clike.Lexer
import qualified Data.Text as T
import Ohua.ALang.Lang
import Ohua.Types
import Ohua.ALang.NS
import qualified Data.HashMap.Strict as HM
import Data.Either
import Data.Maybe
import qualified Ohua.ParseTools.Refs as Refs
import qualified Data.Char as C
import Control.Arrow ((***))

}


%name parseExpH Exp
%name parseNSRaw NS
%tokentype { Lexeme }
%error { parseError }

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
    ','     { Comma }
    '('     { LParen }
    ')'     { RParen }
    '['     { LBracket }
    ']'     { RBracket }
    '{'     { LBrace }
    '}'     { RBrace }
    ';'     { Semicolon }
    ':'     { Colon }
    '::'    { DoubleColon }
    '<'     { LAngleBracket }
    '>'     { RAngleBracket }

%%

QualId 
    : id '::' QualId    { $1 : $3 }
    | id                { [$1] }

NSRef 
    : QualId    { bndsToNSRef $1 }

QualBind
    : QualId    { toQualBnd $1 }

TyExpr 
    : SomeId '<' TyExprList '>' { $3 (TyRef (tyVarFromId $1)) }
    | SomeId                    { TyRef (tyVarFromId $1) }

TyExprList
    : TyExpr ',' TyExprList { $3 . (`TyApp` $1) }
    | TyExpr                { (`TyApp` $1) }

SomeId 
    : id        { Unqual $1 }
    | QualBind  { Qual $1 }

AnnId
    : id TyAnn  { $2 $1 }

TyAnn
    : ':' TyExpr    { Annotated (Just $2) }
    |               { withNoAnn }

Stmts
    : NoSemStmt Stmts   { $1 $2 }
    | StmtSem ';' Stmts { $1 $3 }
    | Exp               { $1 }

NoSemStmt
    : NamedFundef           { let (name, fun) = $1 in Let (Direct $ withNoAnn name) fun }

StmtSem
    : Exp                   { lamWithIgnArg $1 }
    | let Destruct '=' Exp  { Let $2 $4 }

Exp 
    : fn Fundef                             { $2 }
    | SomeId '(' Apply ')'                  { $3 (Var $1) }
    | SomeId                                { Var $1 }
    | for Destruct in Exp '{' Stmts '}'     { Refs.smapBuiltin `Apply` Lambda $2 $6 `Apply` $4 }
    | if '(' Exp ')' '{' Stmts '}' else '{' Stmts '}' 
        { Refs.ifBuiltin `Apply` $3 `Apply` lamWithIgnArg $6 `Apply` lamWithIgnArg $10 }
    | '{' Stmts '}'                         { $2 }

Destruct 
    : AnnId         { Direct $1 :: AbstractAssignment (Annotated (Maybe DefaultTyExpr) Binding) }
    | '[' Vars ']'  { Destructure $2 }

Vars 
    : AnnId ',' Vars    { $1 : $3 }
    | AnnId             { [$1] }

Apply
    : ApplyParams   { $1 }
    |               { id }

ApplyParams
    : Exp ',' Apply  { $3 . (`Apply` $1) }
    | Exp            { (`Apply` $1) }

Fundef 
    : '(' Params ')' '{' Stmts '}' { $2 $5 }

Params
    : HasParams { $1 }
    |           { lamWithIgnArg }

HasParams
    : Destruct ',' HasParams    { Lambda $1 . $3 }
    | Destruct                  { Lambda $1 }

NamedFundef
    : fn id Fundef { (withNoAnn $2, $3) }

NS 
    : ns NSRef ';' Defs { mkNS $2 $4 }

Defs 
    : Def Defs  { $1 : $2 }
    | Def       { [$1] }

Def : use sf Reqdefs ';'    { Left (Left $3) }
    | use algo Reqdefs ';'  { Left (Right $3) }
    | NamedFundef           { Right $1 }

Reqdefs 
    : ReqDef ',' Reqdefs    { $1 : $3 }
    | ReqDef                { [$1] }

ReqDef 
    : NSRef '(' Refers ')' { ($1, $3) }
    | NSRef                { ($1, []) }

Refers 
    : id ',' Refers         { $1 : $3 }
    | id                    { [$1] }
    |                       { [] }

{
lamWithIgnArg ::  AExpr (Annotated (Maybe a) Binding) b -> AExpr (Annotated (Maybe a) Binding) b
lamWithIgnArg = Lambda (Direct (withNoAnn "_"))


withNoAnn :: a -> Annotated (Maybe ann) a
withNoAnn a = Annotated Nothing a


bndsToNSRef :: [Binding] -> NSRef
bndsToNSRef = nsRefFromList

tyVarFromId :: SomeBinding -> SomeTyVar
tyVarFromId bnd | C.isUpper (T.head $ unBinding lbnd) = TyCon bnd
  where lbnd = case bnd of Qual q -> qbName q; Unqual bnd -> bnd;
tyVarFromId bnd = TyVar bnd

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
parseNS :: [Lexeme] -> Namespace SomeBinding
parseNS = parseNSRaw

mkNS :: NSRef -> [Either (Either [(NSRef, [Binding])] [(NSRef, [Binding])]) (Annotated (Maybe DefaultTyExpr) Binding, OptTyAnnExpr SomeBinding)] -> Namespace SomeBinding
mkNS name defs = Namespace name (concat algoRequires) (concat sfRequires) fundefs
  where
    (requireList, fundefList) = partitionEithers defs
    (sfRequires, algoRequires) = partitionEithers requireList
    fundefs = HM.fromList $ map ((\(Annotated v _) -> v) *** removeTyAnns) fundefList

}
