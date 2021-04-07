module Text.WebIDL.Types.Argument

import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Type
import Generics.Derive

%language ElabReflection

||| ConstValue ::
|||     BooleanLiteral
|||     FloatLiteral
|||     integer
||| 
||| BooleanLiteral ::
|||     true
|||     false
public export
data ConstValue = B Bool | F FloatLit | I IntLit

%runElab derive "ConstValue" [Generic,Meta,Eq,Show]

||| Default ::
|||     = DefaultValue
|||     ε
||| 
||| (part of Default)] DefaultValue ::
|||     ConstValue
|||     string
|||     [ ]
|||     { }
|||     null
public export
data Default = None
             | EmptyList
             | EmptySet
             | Null
             | S StringLit
             | C ConstValue

%runElab derive "Default" [Generic,Meta,Eq,Show]

||| ArgumentName ::
|||     ArgumentNameKeyword
|||     identifier
public export
record ArgumentName where
  constructor MkArgName
  value : String

%runElab derive "ArgumentName" [Generic,Meta,Eq,Show]

public export
record Arg where
  constructor MkArg
  attrs    : ExtAttributeList
  type     : IdlType
  name     : ArgumentName

%runElab derive "Text.WebIDL.Types.Argument.Arg" [Generic,Meta,Eq,Show]

public export
record OptArg where
  constructor MkOptArg
  attrs     : ExtAttributeList
  typeAttrs : ExtAttributeList
  type      : IdlType
  name      : ArgumentName
  def       : Default

%runElab derive "OptArg" [Generic,Meta,Eq,Show]

||| ArgumentList ::
|||     Argument Arguments
|||     ε
||| 
||| Arguments ::
|||     , Argument Arguments
|||     ε
||| 
||| Argument ::
|||     ExtendedAttributeList ArgumentRest
|||
||| Ellipsis ::
|||     ...
|||     ε
||| ArgumentRest ::
|||     optional TypeWithExtendedAttributes ArgumentName Default
|||     Type Ellipsis ArgumentName
public export
data ArgumentList : Type where
  VarArg : (args : List Arg) -> (vararg : Arg) -> ArgumentList
  NoVarArg : (args : List Arg) -> (optArgs : List OptArg) -> ArgumentList

%runElab derive "ArgumentList" [Generic,Meta,Eq,Show]
