module Text.WebIDL.Types.StringLit

import Generics.Derive

%language ElabReflection

||| A quoted string literal
public export
record StringLit where
  constructor MkStrLit
  value : String

%runElab derive "StringLit" [Generic,Meta,Eq,Show]
