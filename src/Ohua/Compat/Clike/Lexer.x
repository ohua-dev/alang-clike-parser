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
import Control.Monad.Loops
}

%wrapper "monad-bytestring"

$char = [a-zA-Z]
$sym  = [_]
$numerical = [0-9]
$reserved = [@\#\-\$]
$idchar = [$numerical $sym $char]
$sep = $white

@id = $char $idchar*

:-

    <0> {
        "("         { direct LParen }
        ")"         { direct RParen }
        "["         { direct LBracket }
        "]"         { direct RBracket }
        "{"         { direct LBrace }
        "}"         { direct RBrace }
        "="         { direct OpEq }
        ","         { direct Comma }
        ";"         { direct Semicolon }
        "::"        { direct DoubleColon }
        ":"         { direct Colon }
        "fn"        { direct KWFn }
        "if"        { direct KWIf }
        "else"      { direct KWElse }
        "for"       { direct KWFor }
        "in"        { direct KWIn }
        "use"       { direct KWUse }
        "sf"        { direct KWSf }
        "algo"      { direct KWAlgo }
        "ns"        { direct KWNS }
        "let"       { direct KWLet }
        "<"         { direct LAngleBracket }
        ">"         { direct RAngleBracket }
        @id         { tokenOverInputStr (Id . convertId) }
        $sep        ;

        "/*" { begin blockComment }
        "//" { begin lineComment }
    }

    <lineComment> {
        \n { begin 0 }
        . ;
    }

    <blockComment> {
        "*/" { begin 0 }
        . ;
        \n ;
    }

    $reserved { withMatchedInput $ \s -> alexError $ "Reserved symbol: " ++ BS.unpack s }


{

data Lexeme
    = LParen -- ^ @(@
    | RParen -- ^ @)@
    | LBracket -- ^ @[@
    | RBracket -- ^ @]@
    | LBrace -- ^ @{@
    | RBrace -- ^ @}@
    | LAngleBracket -- ^ @<@
    | RAngleBracket -- ^ @>@
    | OpEq -- ^ @=@
    | Comma -- ^ @,@
    | Semicolon -- ^ @;@
    | Colon -- ^ @:@
    | DoubleColon -- ^ @::@
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
    | Id Binding
    deriving Show


direct tok _ _ = pure $ Just tok

tokenOverInputStr f = withMatchedInput (pure . Just . f)

withMatchedInput f (_, _, input, _) len = f (BS.take len input)

convertId :: ByteString.ByteString -> Binding
convertId = Binding . L.decodeUtf8 . BS.toStrict


toQualId :: ByteString.ByteString -> [Binding]
toQualId = map Binding . T.splitOn "::" . L.decodeUtf8 . BS.toStrict


alexEOF = pure Nothing


-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize bs = either error id $ runAlex bs $ unfoldM alexMonadScan

}
