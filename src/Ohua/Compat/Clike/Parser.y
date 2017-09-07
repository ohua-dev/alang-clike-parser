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

%%

Stmts
    : NoSemStmt Stmts   { $1 $2 }
    | StmtSem ';' Stmts { $1 $3 }
    | Exp               { $1 }

NoSemStmt
    : NamedFundef           { let (name, fun) = $1 in Let (Direct name) fun }

StmtSem
    : Exp                   { Let "_" $1 }
    | let Destruct '=' Exp  { Let $2 $4 }

Exp 
    : fn Fundef                         { $2 }
    | id '(' Apply ')'                  { $3 (Var $1) }
    | id                                { Var $1 }
    | for Destruct in Exp '{' Stmts '}'  { "ohua.lang/smap" `Apply` Lambda $2 $6 `Apply` $4 }
    | if '(' Exp ')' '{' Stmts '}' else '{' Stmts '}' 
        { "ohua.lang/if" `Apply` $3 `Apply` Lambda "_" $6 `Apply` Lambda "_" $10 }
    | '{' Stmts '}'                     { $2 }

Destruct 
    : id            { Direct $1 }
    | '[' Vars ']'  { Destructure $2 }

Vars 
    : id ',' Vars   { $1 : $3 }
    | id            { [$1] }

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
    |           { Lambda "_" }

HasParams
    : Destruct ',' Params   { Lambda $1 . $3 }
    | Destruct              { Lambda $1 }

NamedFundef
    : fn id Fundef { ($2, $3) }

NS 
    : ns id ';' Defs { mkNS $2 $4 }

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
    : id '(' Refers ')' { ($1, $3) }
    | id                { ($1, []) }

Refers 
    : id ',' Refers { $1 : $3 }
    | id            { [$1] }
    |               { [] }

{


-- | Parse a stream of tokens into a simple ALang expression
parseExp :: [Lexeme] -> Expr Binding
parseExp = parseExpH

parseError :: [Lexeme] -> a
parseError tokens = error $ "Parse error" ++ show tokens


-- | Parse a stream of tokens into a namespace
parseNS :: [Lexeme] -> Namespace Binding
parseNS = parseNSRaw

mkNS :: Binding -> [Either (Either [(Binding, [Binding])] [(Binding, [Binding])]) (Binding, Expr Binding)] -> Namespace Binding
mkNS name defs = Namespace name (concat algoRequires) (concat sfRequires) fundefs (HM.lookup "main" fundefs)
  where
    (requireList, fundefList) = partitionEithers defs
    (sfRequires, algoRequires) = partitionEithers requireList
    fundefs = HM.fromList fundefList

}
