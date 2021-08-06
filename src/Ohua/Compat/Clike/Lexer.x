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
  ( tokenize, Lexeme(..), alexMonadScan, runAlex, Alex, lexerPos, absolutePos, linePos, colPos
  )
  where

import Ohua.Prelude hiding (undefined)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as B
import Data.Functor.Foldable
import Data.List (intercalate)
import qualified GHC.Show
import Data.Text (splitOn)
import Control.Lens (use, (.=))

import Prelude (String, undefined, read)
}

%wrapper "monadUserState-bytestring"

$char = [a-zA-Z]
$sym  = [_]
$num_not_zero = [1-9]
$numerical = [0 $num_not_zero]
$reserved = [@\#:\$]
$idchar = [$numerical $sym $char]
$idstartchar = [$char $sym]
$sep = $white

@id = $idstartchar $idchar*
@qualid = @id (:: @id)+

@number = 0 | $num_not_zero $numerical*

:-

    <0> {
        "("         { direct LParen }
        ")"         { direct RParen }
        "{"         { direct LBrace }
        "}"         { direct RBrace }
        "["         { direct LBracket }
        "]"         { direct RBracket }
        "<="        { direct OpLEq }
	      "<"         { direct LAngle }
        ">="        { direct OpGEq }
	      ">"         { direct RAngle }
        "=="        { direct OpEqEq }
        "!="        { direct OpBangEq }
        "="         { direct OpEq }
        ","         { direct Comma }
        "::"        { direct DoubleColon }
	      ":"         { direct Colon }
        ";"         { direct Semicolon }
	      "->" 	      { direct RArrow }
        "||"        { direct OpPipePipe }
        "|"         { direct Pipe }
        "-"         { direct OPMinus }
        "."         { direct OPDot }
        "#"         { direct OPHash }
        "&&"        { direct OpAmpAmp }
        "&"         { direct OPAmpersand }
        "fn"        { direct KWFn }
        "if"        { direct KWIf }
        "else"      { direct KWElse }
        "for"       { direct KWFor }
        "in"        { direct KWIn }
        "use"       { direct KWUse }
        "sf"        { direct KWSf }
        "algo"      { direct KWAlgo }
        "ns" | "mod"{ direct KWNS }
        "let"       { direct KWLet }
	      "mut" 	    { direct KWMut }
        @id         { tokenOverInputStr $ Id . convertId }
        @number     { tokenOverInputStr $ Number . read . BS.unpack }
        $sep        ;
        $reserved   { withMatchedInput $ \s -> alexError $ "Reserved symbol: " <> BS.unpack s }

        "/*#" { beginRecord `andBegin` blockComment }
        "/*"  { begin blockComment }
        "//#" { beginRecord `andBegin` lineComment }
        "//"  { begin lineComment }
    }

    <lineComment> {
        \n { \inp len -> alexSetStartCode 0 >> endRecord inp len >>= maybe alexMonadScan (pure . Pragma) }
        . ;
    }

    <blockComment> {
        "#*/" { \inp len -> alexSetStartCode 0 >> endRecord inp len >>= maybe (throwError "Unexpected closing pragma") (pure . Pragma) }
        "*/"  { \inp len -> alexSetStartCode 0 >> endRecord inp len >>= maybe alexMonadScan (const $ throwError "Unclosed pragma") }
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
    | OPMinus
    | OPDot
    | OPAmpersand
    | OpAmpAmp
    | OpPipePipe
    | OpLEq
    | OpGEq
    | OpEqEq
    | OpBangEq
    | OPHash -- ^ @#@
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
    | Number Integer
    | Id !Binding
    | Pragma Text
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
    OPDot -> "'.'"
    OPHash -> "'#'"
    OPAmpersand -> "'&'"
    DoubleColon -> "'::'"
    Colon -> "':'"
    Semicolon -> "';'"
    Pipe -> "'|'"
    OPMinus -> "'-'"
    OpAmpAmp -> "&&"
    OpPipePipe -> "||"
    OpLEq -> "<="
    OpGEq -> ">="
    OpEqEq -> "=="
    OpBangEq -> "!="
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
    Number n -> "'" <> show n <> "'"
    Id bnd -> "id '" <> bndToString bnd <> "'"
    Pragma t -> "pragma " <> show t
    EOF -> "end of file"
    where
      bndToString = toString . unwrap

data AlexUserState = AlexUserState (Maybe (ByteString.ByteString, Int64) )

instance MonadState AlexState Alex where
    state f = Alex (pure . swap . f)

recordStr f (AlexUserState s) = AlexUserState <$> f s
userState f s = (\u -> s { alex_ust = u }) <$> f ( alex_ust s )
lexerPos f s = (\p -> s { alex_pos = p }) <$> f ( alex_pos s )
alexInput f s = (\p -> s { alex_inp = p }) <$> f ( alex_inp s )
absolutePos f (AlexPn a b c) = (\p -> AlexPn p b c) <$> f a
linePos f (AlexPn a b c) = (\p -> AlexPn a p c) <$> f b
colPos f (AlexPn a b c) = (\p -> AlexPn a b p) <$> f c

beginRecord _ len = do
    p <- use $ lexerPos . absolutePos
    u <- use $ userState . recordStr
    unless (u == Nothing) $ throwError "Internal error: Begin record in begin record"
    i <- use alexInput
    userState . recordStr .= Just (i, fromIntegral p)
    alexMonadScan

endRecord :: a -> Int64 -> Alex (Maybe Text)
endRecord _ len =
    use ( userState . recordStr ) >>= \case
        Just (str, pos0) -> do
            p <- use $ lexerPos . absolutePos
            userState . recordStr .= Nothing
            let matched = decodeUtf8 $ str & BS.take (fromIntegral p - pos0 - len)
            pure $ Just $ matched
        Nothing -> pure Nothing

alexInitUserState = AlexUserState Nothing

direct tok = only (pure tok)

onlyError msg = only (alexError msg)

only f _ _ = f

tokenOverInputStr f = withMatchedInput (pure . f)

withMatchedInput f (_, _, input, _) len = f (BS.take len input)

convertId :: BS.ByteString -> Binding
convertId = makeThrow . decodeUtf8


alexEOF = pure EOF

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
