{
-- |
-- Module      : $Header$
-- Description : Lexer for C-like-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Ohua.Compat.Clike.Lexer (tokenize, Lexeme(..)) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as L
import qualified Data.ByteString.Lazy.Char8 as BS
import Ohua.Types
import Prelude hiding (lex)
}

%wrapper "basic-bytestring"

$char = [a-zA-Z]
$sym  = [_\.]
$numerical = [0-9]
$reserved = [@\#:\-\$]
$idchar = [$numerical $sym $char]
$sep = $white

@id = $char $idchar*


:-

    "("         { const LParen }
    ")"         { const RParen }
    "["         { const LBracket }
    "]"         { const RBracket }
    "{"         { const LBrace }
    "}"         { const RBrace }
    "="         { const OpEq }
    ","         { const Comma }
    ";"         { const Semicolon }
    "fn"        { const KWFn }
    "if"        { const KWIf }
    "else"      { const KWElse }
    "for"       { const KWFor }
    "in"        { const KWIn }
    "require"   { const KWRequire }
    "ns"        { const KWNS }
    "let"       { const KWLet }
    @id         { Id . Binding . L.decodeUtf8 . BS.toStrict }
    $sep        ;

    $reserved { \s -> error $ "Reserved symbol: " ++ BS.unpack s }


{

data Lexeme
    = LParen -- ^ @(@
    | RParen -- ^ @)@
    | LBracket -- ^ @[@
    | RBracket -- ^ @]@
    | LBrace -- ^ @{@
    | RBrace -- ^ @}@
    | OpEq -- ^ @=@
    | Comma -- ^ @,@
    | Semicolon -- ^ @;@
    | KWLet -- ^ keyword @let@
    | KWIf -- ^ keyword @if@
    | KWElse -- ^ keyword @else@
    | KWFor -- ^ keyword @for@
    | KWIn -- ^ keyword @in@
    | KWFn  -- ^ keyword @fn@
    | KWRequire -- ^ keyword @require@
    | KWNS -- ^ keyword @ns@ (namespace)
    | KWReturn -- ^ keyword @return@
    | Id Binding -- ^ an identifier
    deriving Show


-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize = alexScanTokens

}
