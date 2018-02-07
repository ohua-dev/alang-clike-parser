{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Ohua.Compat.Clike.Types where

import           Ohua.Types


type Mutability = Bool

pattern Mutable v = Annotated True v
pattern Immutable v = Annotated False v

type RustTyExpr = Annotated Mutability (TyExpr SomeBinding)

tupleConstructor :: TyExpr SomeBinding
tupleConstructor = TyRef $ Qual $ QualifiedBinding [] "(,)"
