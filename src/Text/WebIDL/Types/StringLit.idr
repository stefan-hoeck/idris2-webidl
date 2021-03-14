module Text.WebIDL.Types.StringLit

import Generics.Derive

%language ElabReflection

||| A quoted string literal
public export
data StringLit = MkStringLit String

%runElab derive "StringLit" [Generic,Meta,Eq,Show]
