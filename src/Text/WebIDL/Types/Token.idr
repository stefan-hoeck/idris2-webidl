module Text.WebIDL.Types.Token

import Derive.Prelude
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Symbol

%default total

%language ElabReflection

||| Text tokens in the WebIDL grammar. The `Invalid` token
||| is not recognized by any parser and will lead to a
||| failure during parsing.
public export
data IdlToken : Type where
  ||| Any number of white space characters
  Space     : IdlToken

  ||| A string literal
  SLit      : StringLit  -> IdlToken

  ||| An integer literal (in decimal, hexadecimal, or
  ||| octal representation)
  ILit      : IntLit     -> IdlToken

  ||| A floating point literal
  FLit      : FloatLit   -> IdlToken

  ||| Any valid identifier that is not also a
  ||| keyword.
  Ident     : Identifier -> IdlToken

  ||| A WebIDL keyword
  Key       : Keyword    -> IdlToken

  ||| A single- or multiline comment
  Comment   : String     -> IdlToken

  ||| A single symbol character (or an ellipsis: ...)
  Other     : Symbol     -> IdlToken

  ||| An unrecognized - and therefore invalid - text
  ||| token.
  Invalid   : String     -> IdlToken

%runElab derive "IdlToken" [Eq,Show]
