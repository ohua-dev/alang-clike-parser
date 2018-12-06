{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Ohua.Prelude

import qualified Data.ByteString.Lazy as B
import Ohua.Frontend.Lang
import Ohua.Frontend.NS
import Ohua.Compat.Clike.Parser
import Ohua.Compat.Clike.Types
import Test.Hspec


lp :: LByteString -> Expr
lp = parseExp

main :: IO ()
main =
    hspec $
    describe "parser and lexer" $ do
        it "parse an apply" $
            lp "something(a, b, c)" `shouldBe` AppE "something" ["a", "b", "c"]
        it "parses a tuple construction" $
            lp "(a, b, c)" `shouldBe` TupE ["a", "b", "c"]
        describe "let" $ do
            it "basic" $ lp "{ let a = b; b }" `shouldBe` LetE "a" "b" "b"
            it "with destructuring" $
                lp "{ let (a, b) = c; a }" `shouldBe` LetE ["a", "b"] "c" "a"
            it "with an ignored binding" $
                lp "{ let _ = a ; a}" `shouldBe` LetE "_" "a" "a"
            it "a sequence of bindings" $
                lp "{ let a = t; let b = r; print(f); a }" `shouldBe`
                LetE "a" "t" (LetE "b" "r" $ StmtE (AppE "print" ["f"]) "a")
        describe "literals" $ do
            it "parses unit" $ lp "()" `shouldBe` LitE UnitLit
            it "parses integers" $ do
                lp "0" `shouldBe` LitE (NumericLit 0)
                lp "1" `shouldBe` LitE (NumericLit 1)
                lp "4" `shouldBe` LitE (NumericLit 4)
                lp "100040" `shouldBe` LitE (NumericLit 100040)
            it "parses negative integers" $ do
                lp "-1" `shouldBe` LitE (NumericLit (-1))
                lp "- 1" `shouldBe` LitE (NumericLit (-1))
                lp "-4" `shouldBe` LitE (NumericLit (-4))
                lp "-100040" `shouldBe` LitE (NumericLit (-100040))
            it "() is a valid pattern" $
                lp "|()| a" `shouldBe` LamE [UnitP] "a"
        it "parses a statement" $
            lp "{ a () ; a }" `shouldBe` StmtE (AppE "a" []) "a"
        it "parses a void block" $
            lp "{ a (); b (); }" `shouldBe`
            StmtE (AppE "a" []) (StmtE (AppE "b" []) $ LitE UnitLit)
        it "parses a lambda" $
            lp "|a, (b, c)| { print(a); c }" `shouldBe`
            LamE ["a", ["b", "c"]] (StmtE (AppE "print" ["a"]) "c")
        -- not sure this next test case is a good idea in a c-like language
        -- we may want operators at some point
        -- it "parses an identifier with strange symbols" $
        --     lp "(let [a-b a0] -)" `shouldBe` Let "a-b" "a0" "-"
        it "parses a named fundef that returns a tuple" $
            parseTLFunDef "fn f () { (a, b) }" `shouldBe`
            Algo
                "f"
                (FunAnn [] (Immutable tupleConstructor))
                (LamE [] $ TupE ["a", "b"])
        -- it "parses an if" $
        --     lp "if (add (x, y)) { fn a () { return b; } return a; } else { return c; }"
        --         `shouldBe`
        describe "comments" $ do
            it "ignores a line comment preceding an expression" $
                lp "// some content\na" `shouldBe` "a"
            it "ignores consecutive line comments" $
                lp "// some content\n      //something\na" `shouldBe` "a"
            it "ignores a line comment following an expression" $
                lp "a // some content" `shouldBe` "a"
            it "ignores a line comment between an expression" $ do
                lp "a(// some content\n b)" `shouldBe` (AppE "a" ["b"])
                lp "a// some content\n( b)" `shouldBe` (AppE "a" ["b"])
            it "ignores a block comment preceding an expression" $ do
                lp "/* ignore this */ a" `shouldBe` "a"
            it "ignores a block comment following an expression" $ do
                lp "a /* ignore this */" `shouldBe` "a"
            it "ignores a block comment in between an expression" $ do
                lp "a /* ignore this */(b)" `shouldBe` (AppE "a" ["b"])
                lp "a (/* ignore this */b)" `shouldBe` (AppE "a" ["b"])
                lp "a (b/* ignore this */)" `shouldBe` (AppE "a" ["b"])
            it "ignores a block comment with newline preceding an expression" $ do
                lp "/* ignore this\n */ a" `shouldBe` "a"
        describe "functions with no inputs" $ do
            it "parses a simple call" $ lp "a()" `shouldBe` (AppE "a" [])
            it "parses chains" $ lp "a(b())" `shouldBe` AppE "a" [AppE "b" []]
        it "parses a function with signature" $
            (parseTLFunDef
                 "fn func (x : int, y : Maybe<String>, z : T<B, a>) -> Q { x }") `shouldBe`
            Algo
                "func"
                (FunAnn
                     [ Immutable $ TyRef "int"
                     , Immutable $ TyRef "Maybe" `TyApp` TyRef "String"
                     , Immutable $
                       (TyRef "T" `TyApp` TyRef "B") `TyApp` TyRef "a"
                     ]
                     (Immutable $ TyRef "Q"))
                (LamE ["x", "y", "z"] $ VarE "x")
        it "parses mutable arguments" $
            (parseTLFunDef "fn func (x : mut Object, y:mut O) -> Q { x }") `shouldBe`
            Algo
                "func"
                (FunAnn
                     [Mutable $ TyRef "Object", Mutable $ TyRef "O"]
                     (Immutable $ TyRef "Q"))
                (LamE ["x", "y"] $ VarE "x")
        it "parses no return as unit" $
            (parseTLFunDef "fn f () { x }") `shouldBe`
            Algo
                "f"
                (FunAnn [] (Immutable tupleConstructor))
                (LamE [] (VarE "x"))
        it "parses tuples in return types" $
            (parseTLFunDef "fn func () -> (A, B) { x }") `shouldBe`
            Algo
                "func"
                (FunAnn
                     []
                     (Immutable
                          (tupleConstructor `TyApp` TyRef "A" `TyApp` TyRef "B")))
                (LamE [] (VarE "x"))
        it "parses the example module" $
            (parseNS <$> B.readFile "test-resources/something.ohuac") `shouldReturn`
            ((emptyNamespace ["some_ns"] :: Namespace ()) &
             algoImports .~ [(["some", "module"], ["a"])] &
             sfImports .~ [(["ohua", "math"], ["add", "isZero"])] &
             decls .~
             [ ( "square"
               , Annotated
                     (FunAnn [Immutable $ TyRef "int"] (Immutable $ TyRef "int")) $
                 LamE ["x"] (AppE "add" ["x", "x"]))
             , ( "algo1"
               , Annotated
                     (FunAnn
                          [Mutable $ TyRef "T"]
                          (Immutable $
                           tupleConstructor `TyApp` TyRef "A" `TyApp`
                           TyRef "Bool")) $
                 LamE ["someParam"] $
                 LetE "a" (AppE "square" ["someParam"]) $
                 LetE "coll0" (MapE (LamE ["i"] (AppE "square" ["i"])) "coll") $
                 IfE (AppE "isZero" ["a"]) "coll0" "a")
             , ( "main"
               , Annotated
                     (FunAnn
                          [Immutable $ TyRef "SomeType"]
                          (Immutable tupleConstructor)) $
                 LamE ["param"] (AppE "algo0" ["param"]))
             ])
