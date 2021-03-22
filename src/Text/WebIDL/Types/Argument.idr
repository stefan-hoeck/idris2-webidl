module Text.WebIDL.Types.Argument

import Text.WebIDL.Types.Attribute
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

||| Ellipsis ::
|||     ...
|||     ε
||| ArgumentRest ::
|||     optional TypeWithExtendedAttributes ArgumentName Default
|||     Type Ellipsis ArgumentName
public export
data ArgumentRest : Type where
  Optional :  (tpe : Attributed IdlType)
           -> (name : ArgumentName)
           -> (def : Default)
           -> ArgumentRest

  Mandatory : (tpe : IdlType) -> (name : ArgumentName) -> ArgumentRest

  VarArg    : (tpe : IdlType) -> (name : ArgumentName) -> ArgumentRest

%runElab derive "ArgumentRest" [Generic,Meta,Eq,Show]

export
Types ArgumentRest where
  types (Optional tpe name def) = types tpe
  types (Mandatory tpe name)    = types tpe
  types (VarArg tpe name)       = types tpe

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
public export
0 ArgumentList : Type
ArgumentList = List (Attributed ArgumentRest)
