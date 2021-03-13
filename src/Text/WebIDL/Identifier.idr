module Text.WebIDL.Identifier

import Data.List1
import Generics.Derive

%language ElabReflection

||| Identifier
public export
record Identifier where 
  constructor MkIdent
  value : String

%runElab derive "Identifier" [Generic,Meta,Eq,Show]

||| IdentifierList :: identifier Identifiers
public export
IdentifierList : Type
IdentifierList = List1 Identifier

||| Identifiers :: "," identifier Identifiers | ε
public export
Identifiers : Type
Identifiers = List Identifier
