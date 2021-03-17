module Text.WebIDL.Types.Token

import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Symbol

import Generics.Derive

%language ElabReflection

public export
data IdlToken : Type where
  Space     : IdlToken
  SLit      : StringLit  -> IdlToken
  ILit      : IntLit     -> IdlToken
  FLit      : FloatLit   -> IdlToken
  Ident     : Identifier -> IdlToken
  Key       : Keyword    -> IdlToken
  Comment   : String     -> IdlToken
  Other     : Symbol     -> IdlToken
  Invalid   : String     -> IdlToken

%runElab derive "IdlToken" [Generic,Meta,Eq,Show]
