module Text.WebIDL.Types.Identifier

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
||| Identifiers :: "," identifier Identifiers | ε
public export
IdentifierList : Type
IdentifierList = List1 Identifier
