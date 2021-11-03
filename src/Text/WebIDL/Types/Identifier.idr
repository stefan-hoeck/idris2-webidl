module Text.WebIDL.Types.Identifier

import Data.List1
import Generics.Derive
import Language.Reflection.Refined
import Text.PrettyPrint.Prettyprinter

import public Data.So
import public Decidable.Equality

%default total

%language ElabReflection

--------------------------------------------------------------------------------
--          Keyword Predicates
--------------------------------------------------------------------------------

||| Checks, whether a given string corresponds to a WebIDL
||| argument name keyword.
public export
isArgumentNameKeyword : String -> Bool
isArgumentNameKeyword "async"        = True
isArgumentNameKeyword "attribute"    = True
isArgumentNameKeyword "callback"     = True
isArgumentNameKeyword "const"        = True
isArgumentNameKeyword "constructor"  = True
isArgumentNameKeyword "deleter"      = True
isArgumentNameKeyword "dictionary"   = True
isArgumentNameKeyword "enum"         = True
isArgumentNameKeyword "getter"       = True
isArgumentNameKeyword "includes"     = True
isArgumentNameKeyword "inherit"      = True
isArgumentNameKeyword "interface"    = True
isArgumentNameKeyword "iterable"     = True
isArgumentNameKeyword "maplike"      = True
isArgumentNameKeyword "mixin"        = True
isArgumentNameKeyword "namespace"    = True
isArgumentNameKeyword "partial"      = True
isArgumentNameKeyword "readonly"     = True
isArgumentNameKeyword "required"     = True
isArgumentNameKeyword "setlike"      = True
isArgumentNameKeyword "setter"       = True
isArgumentNameKeyword "static"       = True
isArgumentNameKeyword "stringifier"  = True
isArgumentNameKeyword "typedef"      = True
isArgumentNameKeyword "unrestricted" = True
isArgumentNameKeyword _              = False

isAttributeNameKeyword : String -> Bool
isAttributeNameKeyword "async"    = True
isAttributeNameKeyword "required" = True
isAttributeNameKeyword _          = False

||| Checks, whether a given string corresponds to one
||| of the floating point constants "NaN", "Infinity", or
||| "-Infinity".
public export
isFloatKeyword : String -> Bool
isFloatKeyword "NaN"       = True
isFloatKeyword "Infinity"  = True
isFloatKeyword "-Infinity" = True
isFloatKeyword _           = False

||| Checks, whether a given string corresponds to any WebIDL keyword.
public export
isKeyword : String -> Bool
isKeyword "ArrayBuffer"       = True
isKeyword "ByteString"        = True
isKeyword "DOMString"         = True
isKeyword "DataView"          = True
isKeyword "Float32Array"      = True
isKeyword "Float64Array"      = True
isKeyword "FrozenArray"       = True
isKeyword "Int16Array"        = True
isKeyword "Int32Array"        = True
isKeyword "Int8Array"         = True
isKeyword "ObservableArray"   = True
isKeyword "Promise"           = True
isKeyword "USVString"         = True
isKeyword "Uint16Array"       = True
isKeyword "Uint32Array"       = True
isKeyword "Uint8Array"        = True
isKeyword "Uint8ClampedArray" = True
isKeyword "any"               = True
isKeyword "bigint"            = True
isKeyword "boolean"           = True
isKeyword "byte"              = True
isKeyword "double"            = True
isKeyword "false"             = True
isKeyword "float"             = True
isKeyword "long"              = True
isKeyword "null"              = True
isKeyword "object"            = True
isKeyword "octet"             = True
isKeyword "optional"          = True
isKeyword "or"                = True
isKeyword "record"            = True
isKeyword "sequence"          = True
isKeyword "short"             = True
isKeyword "symbol"            = True
isKeyword "true"              = True
isKeyword "undefined"         = True
isKeyword "unsigned"          = True
isKeyword s =  isArgumentNameKeyword s
            || isFloatKeyword s

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
--          AttributeNameKeyword
--------------------------------------------------------------------------------

||| Wrapper for one of the attribute name keywords
public export
record AttributeNameKeyword where
  constructor MkAttributeNameKeyword
  value : String
  0 isValid : So (isAttributeNameKeyword value)

%runElab refinedString "AttributeNameKeyword"

--------------------------------------------------------------------------------
--          Identifier
--------------------------------------------------------------------------------

||| Identifier
public export
record Identifier where
  constructor MkIdent
  value : String

%runElab derive "Identifier" [Generic,Meta,Eq,Ord,Show]

||| IdentifierList :: identifier Identifiers
||| Identifiers :: "," identifier Identifiers | ε
public export
IdentifierList : Type
IdentifierList = List1 Identifier

--------------------------------------------------------------------------------
--          Idris Identifier
--------------------------------------------------------------------------------

||| Checks, if a String corresponds to an Idris2 keyword.
public export
isIdrisKeyword : String -> Bool
isIdrisKeyword "covering"       = True
isIdrisKeyword "data"           = True
isIdrisKeyword "do"             = True
isIdrisKeyword "default"        = True
isIdrisKeyword "else"           = True
isIdrisKeyword "export"         = True
isIdrisKeyword "if"             = True
isIdrisKeyword "implementation" = True
isIdrisKeyword "interface"      = True
isIdrisKeyword "module"         = True
isIdrisKeyword "mutual"         = True
isIdrisKeyword "namespace"      = True
isIdrisKeyword "open"           = True
isIdrisKeyword "parameters"     = True
isIdrisKeyword "partial"        = True
isIdrisKeyword "private"        = True
isIdrisKeyword "prefix"         = True
isIdrisKeyword "public"         = True
isIdrisKeyword "record"         = True
isIdrisKeyword "then"           = True
isIdrisKeyword "total"          = True
isIdrisKeyword "using"          = True
isIdrisKeyword "where"          = True
isIdrisKeyword _                = False

||| Wrapper type making sure that no Idris2 keyword
||| is used as a function's name
public export
data IdrisIdent : Type where
  ||| A string that was successfully checked to be not
  ||| an Idris2 keyword. This can be used without further
  ||| adjustments as an Idris2 identifier.
  II         :  (v : String)
             -> (0 _ : isIdrisKeyword v = False)
             -> IdrisIdent

  ||| Primitive function name. This will be prefixed by
  ||| "prim__" during code generation. As such, the resulting
  ||| identifier always valid in idris.
  Prim       : (v : String) -> IdrisIdent

  ||| Fallback constructor for Idris2 keywords. An underscore
  ||| will be appended to the wrapped string during code generation.
  Underscore : (v : String) -> IdrisIdent

export
Show IdrisIdent where
  show (II v _)       = case strM v of
                             StrCons '_' xs => xs
                             _              => v
  show (Prim v)       = "prim__" ++ v
  show (Underscore v) = v ++ "_"

export
FromString IdrisIdent where
  fromString s with (decEq (isIdrisKeyword s) False)
    fromString s | Yes refl = II s refl
    fromString s | No _     = Underscore s

export
Pretty IdrisIdent where
  pretty = pretty . show

export
fromIdent : Identifier -> IdrisIdent
fromIdent = fromString . value
