module Ohua.Compat.Clike.Types where

import Ohua.Prelude


type Mutability = Bool

pattern Mutable :: value -> Annotated Bool value
pattern Mutable v = Annotated True v
pattern Immutable :: value -> Annotated Bool value
pattern Immutable v = Annotated False v

type RustTyExpr = Annotated Mutability (TyExpr SomeBinding)

tupleConstructor :: TyExpr SomeBinding
tupleConstructor = TyRef $ Qual $ QualifiedBinding [] "(,)"
