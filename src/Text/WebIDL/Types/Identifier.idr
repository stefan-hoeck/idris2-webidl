module Text.WebIDL.Types.Identifier

import Data.List1
import Generics.Derive
import Language.Reflection.Refined
import public Data.So

%language ElabReflection

--------------------------------------------------------------------------------
--          Keyword Predicates
--------------------------------------------------------------------------------

public export
isArgumentNameKeyword : String -> Bool
isArgumentNameKeyword "async" = True
isArgumentNameKeyword "attribute" = True
isArgumentNameKeyword "callback" = True
isArgumentNameKeyword "const" = True
isArgumentNameKeyword "constructor" = True
isArgumentNameKeyword "deleter" = True
isArgumentNameKeyword "dictionary" = True
isArgumentNameKeyword "enum" = True
isArgumentNameKeyword "getter" = True
isArgumentNameKeyword "includes" = True
isArgumentNameKeyword "inherit" = True
isArgumentNameKeyword "interface" = True
isArgumentNameKeyword "iterable" = True
isArgumentNameKeyword "maplike" = True
isArgumentNameKeyword "mixin" = True
isArgumentNameKeyword "namespace" = True
isArgumentNameKeyword "partial" = True
isArgumentNameKeyword "readonly" = True
isArgumentNameKeyword "required" = True
isArgumentNameKeyword "setlike" = True
isArgumentNameKeyword "setter" = True
isArgumentNameKeyword "static" = True
isArgumentNameKeyword "stringifier" = True
isArgumentNameKeyword "typedef" = True
isArgumentNameKeyword "unrestricted" = True
isArgumentNameKeyword _ = False

public export
isKeyword : String -> Bool
isKeyword "ArrayBuffer" = True
isKeyword "ByteString" = True
isKeyword "DOMString" = True
isKeyword "DataView" = True
isKeyword "Float32Array" = True
isKeyword "Float64Array" = True
isKeyword "FrozenArray" = True
isKeyword "Int16Array" = True
isKeyword "Int32Array" = True
isKeyword "Int8Array" = True
isKeyword "ObservableArray" = True
isKeyword "Promise" = True
isKeyword "USVString" = True
isKeyword "Uint16Array" = True
isKeyword "Uint32Array" = True
isKeyword "Uint8Array" = True
isKeyword "Uint8ClampedArray" = True
isKeyword "any" = True
isKeyword "bigint" = True
isKeyword "boolean" = True
isKeyword "byte" = True
isKeyword "double" = True
isKeyword "false" = True
isKeyword "float" = True
isKeyword "long" = True
isKeyword "null" = True
isKeyword "object" = True
isKeyword "octet" = True
isKeyword "optional" = True
isKeyword "or" = True
isKeyword "record" = True
isKeyword "sequence" = True
isKeyword "short" = True
isKeyword "symbol" = True
isKeyword "true" = True
isKeyword "undefined" = True
isKeyword "unsigned" = True
isKeyword s = isArgumentNameKeyword s

--------------------------------------------------------------------------------
--          Keyword
--------------------------------------------------------------------------------

public export
record Keyword where
  constructor MkKeyword
  value : String
  0 isValid : So (isKeyword value)

%runElab refinedString "Keyword"

--------------------------------------------------------------------------------
--          ArgumentNameKeyword
--------------------------------------------------------------------------------

public export
record ArgumentNameKeyword where
  constructor MkArgumentNameKeyword
  value : String
  0 isValid : So (isArgumentNameKeyword value)

%runElab refinedString "ArgumentNameKeyword"

--------------------------------------------------------------------------------
--          Identifier
--------------------------------------------------------------------------------

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
