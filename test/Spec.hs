{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Data.ByteString.Lazy     as B
import           Ohua.ALang.Lang
import           Ohua.ALang.NS
import           Ohua.Compat.Clike.Lexer
import           Ohua.Compat.Clike.Parser
import           Ohua.Types
import           Test.Hspec

deriving instance Show a => Show (Namespace a)
deriving instance Eq a => Eq (Namespace a)

lp = parseExp . tokenize

main :: IO ()
main = hspec $ do
    describe "parser and lexer" $ do
        it "parse an apply" $
            lp "something(a, b, c)" `shouldBe` ("something" `Apply` "a" `Apply` "b" `Apply` "c")
        it "parses a let" $
            lp "{ let a = b; b }" `shouldBe` Let "a" "b" "b"
        it "parses a lambda" $
            lp "fn (a, [b, c]) { print(a); c }" `shouldBe` (Lambda "a" $ Lambda (Destructure ["b", "c"]) $ Let "_" ("print" `Apply` "a") "c")
        -- not sure this next test case is a good idea in a c-like language
        -- we may want operators at some point
        -- it "parses an identifier with strange symbols" $
        --     lp "(let [a-b a0] -)" `shouldBe` Let "a-b" "a0" "-"
        it "parses sequences of let binds" $
            lp "{ let a = t; let b = r; print(f); a }" `shouldBe` (Let "a" "t" $ Let "b" "r" $ Let "_" ("print" `Apply` "f") "a")
        it "parses a named fundef" $
            lp "{ fn a_fn (a) { a } b }" `shouldBe` (Let "a_fn" (Lambda "a" "a") "b")
        -- it "parses an if" $
        --     lp "if (add (x, y)) { fn a () { return b; } return a; } else { return c; }"
        --         `shouldBe`
        it "parses the example module" $ (parseNS . tokenize <$> B.readFile "test-resources/something.ohuac")
            `shouldReturn`
            Namespace (nsRefFromList ["some_ns"])
                [ (nsRefFromList ["some","module"], ["a"]) ]
                [ (nsRefFromList ["ohua","math"],["add","isZero"]) ]
                [ ("square", Lambda "x" ("add" `Apply` "x" `Apply` "x"))
                , ("algo1", Lambda "someParam" $
                        Let "a" ("square" `Apply` "someParam") $
                        Let "coll0" ("ohua.lang/smap" `Apply` Lambda "i" ("square" `Apply` "i") `Apply` "coll") $
                        ("ohua.lang/if"
                            `Apply` ("isZero" `Apply` "a")
                            `Apply` Lambda "_" "coll0"
                            `Apply` Lambda "_" "a"))
                , ("main", Lambda "param"  ("algo0" `Apply` "param"))
                ]
