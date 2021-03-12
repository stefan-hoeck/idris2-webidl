module Text.WebIDL.StringLit

import Generics.Derive

%language ElabReflection

||| A quoted string literal
public export
data StringLit = MkStringLit String

%runElab derive "StringLit" [Generic,Meta,Eq,Show]
