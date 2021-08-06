{
-- |
-- Module      : $Header$
-- Description : Parser for ALang S-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings, LambdaCase, TypeApplications #-}
module Ohua.Compat.Clike.Parser
    ( parseNS, parseExp, parseTLFunDef
    , Namespace(..)
    , Algo(..)
    ) where

import Ohua.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BS
import Control.Lens (at, ix, use, non)
import Control.Monad.Writer (runWriter, tell)
import Data.Functor.Foldable (cata, embed)
import Control.Category ((>>>))

import Ohua.Compat.Clike.Lexer
import Ohua.Compat.Clike.Types
import Ohua.Frontend.Lang
import Ohua.Types
import Ohua.Frontend.NS
import Ohua.ALang.PPrint (quickRender)

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
    pragma  { Pragma $$ }

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
    '['     { LBracket }
    ']'     { RBracket }
    '::'    { DoubleColon }
    ':'     { Colon }
    ';'     { Semicolon }
    '<'     { LAngle }
    '>'     { RAngle }
    '->'    { RArrow }
    '|'     { Pipe }
    '-'     { OPMinus }
    '.'     { OPDot }
    '#'     { OPHash }
    '&'     { OPAmpersand }
    '=='    { OpEqEq }
    '!='    { OpBangEq }
    '<='    { OpLEq }
    '>='    { OpGEq }
    '||'    { OpPipePipe }
    '&&'    { OpAmpAmp }

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

tuple(p)
    : parens(many_sep(p, ',')) { $1 }

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

SimpleNoTuple
    : Block                                 { $1 }
    | SomeId                                { $1 }
    | opt('-') number            { LitE (NumericLit $ maybe id (const negate) $1 $2) }

SimpleExp :: { RawExpression }
    : tuple(Exp)             { case $1 of
                                   [] -> LitE UnitLit
                                   [x] -> x
                                   xs -> TupE xs }
    | '&' SimpleExp { ohuaLangFun "&" `AppE` [ $2 ] }
    | SimpleNoTuple          { $1 }


BindCont
    : '.' SimpleNoTuple { ( `BindE` $2 ) }
    |                   { id }

CallCont
    : tuple(Exp)  { ( `AppE` $1 ) }
    |             { id }

op
    : '==' { ohuaLangFun "==" }
    | '>=' { ohuaLangFun ">=" }
    | '<=' { ohuaLangFun "<=" }
    | '<'  { ohuaLangFun "<" }
    | '>'  { ohuaLangFun ">" }
    | '||' { ohuaLangFun "||" }
    | '&&' { ohuaLangFun "&&" }
    | '!=' { ohuaLangFun "!=" }

OpCont
    : op SimpleOrCall { \before -> AppE $1 [before, $2] }
    |                { id }

SimpleOrCall
    : SimpleExp BindCont CallCont OpCont { $4 $ ($3 . $2) $1 }

IfCont
    : SimpleOrCall Block ThenCont { IfE $1 $2 $3 }

ThenCont
    : else ElseCont               { $2 }
    |                             { LitE UnitLit }

ElseCont
    : if IfCont                   { $2 }
    | Block                       { $1 }

Exp
    :: { RawExpression }
    : '|' many_sep(Pat, ',') '|' SimpleOrCall  { LamE $2 $4 }
    | for Pat in Exp Block                  { MapE (LamE [$2] $5) $4 }
    | if IfCont                             { $2 }
    | SimpleOrCall                          { $1 }

Pat
    :: { Pat }
    : id                         { VarP $1 }
    | tuple(Pat)                 { case $1 of
                                       [] -> UnitP
                                       [x] -> x
                                       xs -> TupP xs }

TLFunSig
    : fn id tuple(AnnPat) FunRetAnn
        { let (pats, types) = unzip $3
          in (($2, FunAnn types $ Immutable $4), pats) }

TLFunDef
    :: { Algo }
    : TLFunSig Block
        { let ( (name, ann), pats ) = $1
          in Algo { algoName = name
                  , algoTyAnn = ann
                  , algoCode = LamE pats $2
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

NS
    : ns QualId ';' many(Decl) { mkNS (bndsToNSRef $2) $4 }

FunDefCont
    : Block
        { \( (name, ann),  pats ) ->
          AlgoDecl $
          Algo { algoName = name
               , algoTyAnn = ann
               , algoCode = LamE pats $1
               } }

Decl :: { Decl }
    : use ReqDef ';'        { ImportDecl $2 }
    | TLFunSig FunDefCont   { $2 $1 }
    | pragma                { PragmaD $ either error id $ parsePragma $1 }
    | '#' '[' SomeId ']'    { MacroCall $3 }

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

ohuaLangFun = LitE . FunRefLit . flip FunRef Nothing . QualifiedBinding ["ohua", "lang"]

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
  line <- use $ lexerPos . linePos
  col <- use $ lexerPos . colPos
  throwError $ "Parse error at line " <> show line <> ", column " <> show col <> ", on token " <> show token


-- | Parse a stream of tokens into a namespace
parseNS :: Input -> (Namespace (Annotated (FunAnn RustTyExpr) Expr), HashMap Binding [Expr])
parseNS = runPM parseNSRawM

getAnnots :: [Decl] -> (HashMap Binding [Expr])
getAnnots =
    foldl' f ([], mempty)
    >>> (\(a, b) -> if null a then b else error $ "Bare macro at top level " <> quickRender a)
  where
    f (mAnn, anns) = \case
        MacroCall a -> (a:mAnn, anns)
        AlgoDecl Algo { algoName }
            | not (null mAnn) -> ([], at algoName . non [] %~ (mAnn ++) $  anns)
        other | not (null mAnn) ->
            trace ("Warning: Ignoring macro calls " <> quickRender mAnn <> " on invalid declaration " <> quickRender other)
                  ([], anns)
        _ -> (mAnn, anns)

mkNS :: NSRef -> [Decl] -> (Namespace (Annotated (FunAnn RustTyExpr) Expr), HashMap Binding [Expr])
mkNS name defs =
    (emptyNamespace name :: Namespace ())
        & algoImports .~ map importToTuple algoRequires <> map ((,[])) extraAlgoNamespaces
        & sfImports .~ map importToTuple sfRequires <> map ((,[])) extraSfNamespaces
        & decls .~ algos
        & pragmas .~ pragmaList
        & (, annots)
  where
    annots = getAnnots defs
    (requires, algoList, pragmaList) = partitionDecls defs
    sfRequires = filter (not . isAlgo) requires
    algoRequires = filter isAlgo requires
    importToTuple Import{nsRef, bindings} = (nsRef, bindings)
    fillInRefers = cata $ sequence >=> \case
        LitEF (FunRefLit (FunRef ref id))
            | Just (ns, isAlgo) <- ref ^? namespace . unwrapped . ix 0 >>= getRequire -> do
                let newBnd = ref & namespace . unwrapped @NSRef @NSRef %~ (ns ^. unwrapped <>)
                tell $ if isAlgo then ([ newBnd ^. namespace ], mempty) else (mempty, [ newBnd ^. namespace ])
                pure $ LitE (FunRefLit (FunRef newBnd id))
        e -> pure $ embed e
    getRequire = flip HM.lookup $ HM.fromList [ (refer, ( nsRef import_, isAlgo import_ )) | import_ <- sfRequires <> algoRequires, refer <- bindings import_ ]
    (algoList', (extraAlgoNamespaces, extraSfNamespaces)) = runWriter $ traverse (\a@Algo{algoCode=c} -> (\c' -> a { algoCode = c' }) <$> fillInRefers c) algoList
    algos = HM.fromListWith (\a1 a2 -> trace ("Warning: double defined algo \n" <> quickRender a1 <> "\n and \n " <> quickRender a2 <> "\n the former will be ignored!") a2) $ map (\Algo{algoTyAnn, algoName, algoCode} -> (algoName, Annotated algoTyAnn algoCode)) algoList' -- ignores algos which are defined twice

partitionDecls :: [Decl] -> ([Import], [Algo], [Pragma])
partitionDecls = flip foldl' mempty . flip $ \case
    ImportDecl i -> _1 %~ (i:)
    AlgoDecl a -> _2 %~ (a:)
    PragmaD p -> _3 %~ (p:)
    MacroCall _ -> id
}
