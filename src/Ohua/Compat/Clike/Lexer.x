{
-- |
-- Module      : $Header$
-- Description : Lexer for C-like-Expressions
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-unused-imports #-}
module Ohua.Compat.Clike.Lexer
  ( tokenize, Lexeme(..), alexMonadScan, runAlex, Alex, getLexerPos
  )
  where

import Ohua.Prelude hiding (undefined)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as B
import Data.Functor.Foldable
import Data.List (intercalate)
import qualified GHC.Show
import Data.Text (splitOn)

import Prelude (String, undefined, read)
}

%wrapper "monad-bytestring"

$char = [a-zA-Z]
$sym  = [_]
$num_not_zero = [1-9]
$numerical = [0 $num_not_zero]
$reserved = [@\#:\-\$]
$idchar = [$numerical $sym $char]
$idstartchar = [$char $sym]
$sep = $white

@id = $idstartchar $idchar*
@qualid = @id (:: @id)+

@number = 0 | "-"? $num_not_zero $numerical*

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
        "::"        { direct DoubleColon }
	      ":"         { direct Colon }
        ";"         { direct Semicolon }
	      "->" 	      { direct RArrow }
        "|"         { direct Pipe }
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
        "with"      { direct KWWith }
        @id         { tokenOverInputStr $ Id . convertId }
        @number     { tokenOverInputStr $ Number . read . BS.unpack }
        $sep        ;
        $reserved { withMatchedInput $ \s -> alexError $ "Reserved symbol: " <> BS.unpack s }

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
    | DoubleColon -- ^ @::@
    | Colon -- ^ @:@
    | Semicolon -- ^ @;@
    | LAngle -- ^ @<@
    | RAngle -- ^ @>@
    | RArrow -- ^ @->@
    | Pipe
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
    | KWWith
    | Number Integer
    | Id !Binding
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
    DoubleColon -> "'::'"
    Colon -> "':'"
    Semicolon -> "';'"
    Pipe -> "'|'"
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
    KWWith -> "'with'"
    Number n -> "'" <> show n <> "'"
    Id bnd -> "id '" <> bndToString bnd <> "'"
    EOF -> "end of file"
    where
      bndToString = toString . unwrap


direct tok _ _ = pure tok

tokenOverInputStr f = withMatchedInput (pure . f)

withMatchedInput f (_, _, input, _) len = f (BS.take len input)

convertId :: BS.ByteString -> Binding
convertId = makeThrow . decodeUtf8


alexEOF = pure EOF

getLexerPos :: Alex (Int, Int)
getLexerPos = Alex $ \s@AlexState{ alex_pos=AlexPn _ line col} -> pure (s, (line, col))

-- | Tokenize a lazy bytestring into lexemes
tokenize :: BS.ByteString -> [Lexeme]
tokenize bs = either (error . toText) identity $ runAlex bs $
  let go = alexMonadScan >>= \case EOF -> pure []; tok -> (tok:) <$> go
  in go

instance MonadError [Char] Alex where
  throwError = alexError
  catchError ac handler = Alex $ \input ->
    case unAlex ac input of
      Left err -> unAlex (handler err) input
      Right res -> Right res

}
