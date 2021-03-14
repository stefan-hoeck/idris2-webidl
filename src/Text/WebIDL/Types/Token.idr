module Text.WebIDL.Types.Token

import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit

import Generics.Derive

%language ElabReflection

public export
data IdlToken : Type where
  Space     : IdlToken
  StrLit    : StringLit  -> IdlToken
  IntLit    : Integer    -> IdlToken
  FltLit    : FloatLit   -> IdlToken
  Ident     : Identifier -> IdlToken
  Comment   : String     -> IdlToken
  Other     : String     -> IdlToken
  Invalid   : String     -> IdlToken

%runElab derive "IdlToken" [Generic,Meta,Eq,Show]
