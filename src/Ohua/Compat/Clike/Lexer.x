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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Ohua.Compat.Clike.Lexer
  ( tokenize, Lexeme(..), alexMonadScan, runAlex, Alex, getLexerPos
  )
  where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as B
import Ohua.Types
import Prelude hiding (lex)
import Control.Monad.Loops
import qualified Ohua.Util.Str as Str
import Data.Functor.Foldable
import Data.List (intercalate)
import Control.Monad.Error.Class
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
        "{"         { direct LBrace }
        "}"         { direct RBrace }
	"<"         { direct LAngle }
	">"         { direct RAngle }
        "="         { direct OpEq }
        ","         { direct Comma }
	":"         { direct Colon }
        ";"         { direct Semicolon }
	"->" 	    { direct RArrow }
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
	"mut" 	    { direct KWMut }
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
    | Colon -- ^ @:@
    | Semicolon -- ^ @;@
    | LAngle -- ^ @<@
    | RAngle -- ^ @>@
    | RArrow -- ^ @->@
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
    | KWMut -- ^ keyword @mut@
    | UnqualId !Binding
    | QualId ![Binding] -- ^ an identifier
    | EOF

instance Show Lexeme where
  show = \case
    LParen -> "'('"
    RParen -> "')'"
    LBracket -> "'['"
    RBracket -> "']'"
    LBrace -> "'{'"
    RBrace -> "'}'"
    OpEq -> "'='"
    Comma -> "','"
    Colon -> "':'"
    Semicolon -> "';'"
    LAngle -> "'<'"
    RAngle -> "'>'"
    RArrow -> "'->'"
    KWLet -> "'let'"
    KWIf -> "'if'"
    KWElse -> "'else'"
    KWFor -> "'for'"
    KWIn -> "'in'"
    KWFn -> "'fn'"
    KWUse -> "'use'"
    KWAlgo -> "'algo'"
    KWSf -> "'sf'"
    KWNS -> "'ns'"
    KWMut -> "'mut'"
    UnqualId bnd -> "unqualified identifier '" ++ bndToString bnd ++ "'"
    QualId bnds -> "qualified identifier '" ++ intercalate "::" (map bndToString bnds) ++ "'"
    EOF -> "end of file"
    where
      bndToString = Str.toString . unBinding
    

direct tok _ _ = pure tok

tokenOverInputStr f = withMatchedInput (pure . f)

withMatchedInput f (_, _, input, _) len = f (BS.take len input)

convertId :: BS.ByteString -> Binding
convertId = Binding . Str.fromString . BS.unpack


toQualId :: BS.ByteString -> [Binding]
toQualId = map (Binding . Str.fromString . B.unpack) . splitOn "::" . BS.toStrict

splitOn str = ana $ \bs ->
  case B.breakSubstring str bs of
    (b1, b2) | B.null b1 && B.null b2 -> Nil
             | otherwise -> Cons b1 (B.drop (B.length str) b2)

alexEOF = pure EOF

getLexerPos :: Alex (Int, Int)
getLexerPos = Alex $ \s@AlexState{ alex_pos=AlexPn _ line col} -> pure (s, (line, col))

-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize bs = either error id $ runAlex bs $
  let go = alexMonadScan >>= \case EOF -> pure []; tok -> (tok:) <$> go
  in go

instance MonadError [Char] Alex where
  throwError = alexError
  catchError ac handler = Alex $ \input ->
    case unAlex ac input of
      Left err -> unAlex (handler err) input
      Right res -> Right res

}
