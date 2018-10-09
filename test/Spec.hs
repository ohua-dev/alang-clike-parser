{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Ohua.Prelude

import qualified Data.ByteString.Lazy as B
import Ohua.ALang.Lang
import Ohua.ALang.NS
import Ohua.ALang.Refs (mkTuple)
import Ohua.Unit
import Ohua.Compat.Clike.Parser
import Ohua.Compat.Clike.Types
import Test.Hspec


lp :: LByteString -> Expr SomeBinding
lp = parseExp

main :: IO ()
main =
    hspec $
    describe "parser and lexer" $ do
        it "parse an apply" $
            lp "something(a, b, c)" `shouldBe`
            ("something" `Apply` "a" `Apply` "b" `Apply` "c")
        it "parses a tuple construction" $
            lp "(a, b, c)" `shouldBe`
            (Var (Qual mkTuple) `Apply` "a" `Apply` "b" `Apply` "c")
        it "parses a let" $ lp "{ let a = b; b }" `shouldBe` Let "a" "b" "b"
        it "parses a let with destructuring" $
            lp "{ let (a, b) = c; a }" `shouldBe` Let ["a", "b"] "c" "a"
        it "parses a lambda" $
            lp "fn (a, (b, c)) { print(a); c }" `shouldBe`
            (Lambda "a" $
             Lambda (Destructure ["b", "c"]) $ Let "_" ("print" `Apply` "a") "c")
        -- not sure this next test case is a good idea in a c-like language
        -- we may want operators at some point
        -- it "parses an identifier with strange symbols" $
        --     lp "(let [a-b a0] -)" `shouldBe` Let "a-b" "a0" "-"
        it "parses sequences of let binds" $
            lp "{ let a = t; let b = r; print(f); a }" `shouldBe`
            (Let "a" "t" $ Let "b" "r" $ Let "_" ("print" `Apply` "f") "a")
        it "parses a named fundef" $
            lp "{ fn a_fn (a) { a } b }" `shouldBe`
            (Let "a_fn" (Lambda "a" "a") "b")
        it "parses a named fundef that returns a tuple" $
            parseTLFunDef "fn f () { (a, b) }" `shouldBe`
            ( "f"
            , Annotated
                  (FunAnn [] (Immutable tupleConstructor))
                  (Lambda "_" $ Var (Qual mkTuple) `Apply` "a" `Apply` "b"))
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
                lp "a(// some content\n b)" `shouldBe` ("a" `Apply` "b")
                lp "a// some content\n( b)" `shouldBe` ("a" `Apply` "b")
            it "ignores a block comment preceding an expression" $ do
                lp "/* ignore this */ a" `shouldBe` "a"
            it "ignores a block comment following an expression" $ do
                lp "a /* ignore this */" `shouldBe` "a"
            it "ignores a block comment in between an expression" $ do
                lp "a /* ignore this */(b)" `shouldBe` ("a" `Apply` "b")
                lp "a (/* ignore this */b)" `shouldBe` ("a" `Apply` "b")
                lp "a (b/* ignore this */)" `shouldBe` ("a" `Apply` "b")
            it "ignores a block comment with newline preceding an expression" $ do
                lp "/* ignore this\n */ a" `shouldBe` "a"
        describe "functions with no inputs" $ do
            it "parses a simple call" $
                lp "a()" `shouldBe` ("a" `Apply` someUnitExpr)
            it "parses chains" $
                lp "a(b())" `shouldBe` ("a" `Apply` ("b" `Apply` someUnitExpr))
        it "parses a function with signature" $
            (parseTLFunDef
                 "fn func (x : int, y : Maybe<String>, z : T<B, a>) -> Q { x }") `shouldBe`
            ( "func"
            , Annotated
                  (FunAnn
                       [ Immutable $ TyRef "int"
                       , Immutable $ TyRef "Maybe" `TyApp` TyRef "String"
                       , Immutable $
                         (TyRef "T" `TyApp` TyRef "B") `TyApp` TyRef "a"
                       ]
                       (Immutable $ TyRef "Q")) $
              Lambda "x" $ Lambda "y" $ Lambda "z" $ Var "x")
        it "parses mutable arguments" $
            (parseTLFunDef "fn func (x : mut Object, y:mut O) -> Q { x }") `shouldBe`
            ( "func"
            , Annotated
                  (FunAnn
                       [Mutable $ TyRef "Object", Mutable $ TyRef "O"]
                       (Immutable $ TyRef "Q")) $
              Lambda "x" $ Lambda "y" $ Var "x")
        it "parses no return as unit" $
            (parseTLFunDef "fn f () { x }") `shouldBe`
            ( "f"
            , Annotated (FunAnn [] (Immutable tupleConstructor)) $
              Lambda "_" (Var "x"))
        it "parses tuples in return types" $
            (parseTLFunDef "fn func () -> (A, B) { x }") `shouldBe`
            ( "func"
            , Annotated
                  (FunAnn
                       []
                       (Immutable
                            (tupleConstructor `TyApp` TyRef "A" `TyApp`
                             TyRef "B"))) $
              Lambda "_" (Var "x"))
        it "parses the example module" $
            (parseNS <$> B.readFile "test-resources/something.ohuac") `shouldReturn`
            ((emptyNamespace ["some_ns"] :: Namespace ()) &
             algoImports .~ [(["some", "module"], ["a"])] &
             sfImports .~ [(["ohua", "math"], ["add", "isZero"])] &
             decls .~
             [ ( "square"
               , Annotated
                     (FunAnn [Immutable $ TyRef "int"] (Immutable $ TyRef "int")) $
                 Lambda "x" ("add" `Apply` "x" `Apply` "x"))
             , ( "algo1"
               , Annotated
                     (FunAnn
                          [Mutable $ TyRef "T"]
                          (Immutable $
                           tupleConstructor `TyApp` TyRef "A" `TyApp`
                           TyRef "Bool")) $
                 Lambda "someParam" $
                 Let "a" ("square" `Apply` "someParam") $
                 Let
                     "coll0"
                     ("ohua.lang/smap" `Apply` Lambda "i" ("square" `Apply` "i") `Apply`
                      "coll") $
                 ("ohua.lang/if" `Apply` ("isZero" `Apply` "a") `Apply`
                  Lambda "_" "coll0" `Apply`
                  Lambda "_" "a"))
             , ( "main"
               , Annotated
                     (FunAnn
                          [Immutable $ TyRef "SomeType"]
                          (Immutable tupleConstructor)) $
                 Lambda "param" ("algo0" `Apply` "param"))
             ])
