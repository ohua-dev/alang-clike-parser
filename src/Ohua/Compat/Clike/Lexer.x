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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Ohua.Compat.Clike.Lexer (tokenize, Lexeme(..)) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as B
import Ohua.Types
import Prelude hiding (lex)
import Control.Monad.Loops
import qualified Ohua.Util.Str as Str
import Data.Functor.Foldable
}

%wrapper "monad-bytestring"

$char = [a-zA-Z]
$sym  = [_]
$numerical = [0-9]
$reserved = [@\#:\-\$]
$idchar = [$numerical $sym $char]
$sep = $white

@id = $char $idchar*
@qualid = @id (:: @id)+

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
        @qualid     { tokenOverInputStr $ QualId . toQualId }
        @id         { tokenOverInputStr $ UnqualId . convertId }
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
    | UnqualId Binding
    | QualId [Binding] -- ^ an identifier
    deriving Show


direct tok _ _ = pure $ Just tok

tokenOverInputStr f = withMatchedInput (pure . Just . f)

withMatchedInput f (_, _, input, _) len = f (BS.take len input)

convertId :: BS.ByteString -> Binding
convertId = Binding . Str.fromString . BS.unpack


toQualId :: BS.ByteString -> [Binding]
toQualId = map (Binding . Str.fromString . B.unpack) . splitOn "::" . BS.toStrict

splitOn str = ana $ \bs ->
  case B.breakSubstring str bs of
    (b1, b2) | B.null b1 && B.null b2 -> Nil
             | otherwise -> Cons b1 (B.drop (B.length str) b2)

alexEOF = pure Nothing


-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize bs = either error id $ runAlex bs $ unfoldM alexMonadScan

}
