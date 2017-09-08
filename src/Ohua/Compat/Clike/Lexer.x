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
$sym  = [_]
$numerical = [0-9]
$reserved = [@\#:\-\$]
$idchar = [$numerical $sym $char]
$sep = $white

@id = $char $idchar (:: $idchar +)*


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
    "use"       { const KWUse }
    "sf"        { const KWSf }
    "algo"      { const KWAlgo }
    "ns"        { const KWNS }
    "let"       { const KWLet }
    @id         { Id . toId }
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
    | KWUse -- ^ keyword @use@
    | KWAlgo -- ^ keyword @algo@
    | KWSf -- ^ keyword @sf@
    | KWNS -- ^ keyword @ns@ (namespace)
    | Id [Binding] -- ^ an identifier
    deriving Show


convertId :: ByteString.ByteString -> Binding
convertId = Binding . L.decodeUtf8 . BS.toStrict


toId :: ByteString.ByteString -> [Binding]
toId = map Binding . T.splitOn "::" . L.decodeUtf8 . BS.toStrict


-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize = alexScanTokens

}
