module Text.WebIDL.Types.Member

import Generics.Derive
import Text.WebIDL.Types.Argument
import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Type

%language ElabReflection

||| Const ::
|||     const ConstType identifier = ConstValue ;
||| 
||| ConstValue ::
|||     BooleanLiteral
|||     FloatLiteral
|||     integer
public export
record Const where
  constructor MkConst
  type  : ConstType
  name  : Identifier
  value : ConstValue

%runElab derive "Const" [Generic,Meta,Eq,Show]

--------------------------------------------------------------------------------
--          Operation
--------------------------------------------------------------------------------

||| OperationName ::
|||     OperationNameKeyword
|||     identifier
||| 
||| OperationNameKeyword ::
|||     includes
public export
record OperationName where
  constructor MkOpName
  value : String

%runElab derive "OperationName" [Generic,Meta,Eq,Show]

||| Special ::
|||     getter
|||     setter
|||     deleter
public export
data Special = Getter | Setter | Deleter

%runElab derive "Special" [Generic,Meta,Eq,Show]

||| RegularOperation ::
|||     Type OperationRest
||| 
||| OperationRest ::
|||     OptionalOperationName ( ArgumentList ) ;
||| 
||| OptionalOperationName ::
|||     OperationName
|||     ε
|||
||| SpecialOperation ::
|||     Special RegularOperation
|||
||| Operation ::
|||     RegularOperation
|||     SpecialOperation
public export
record Op a where
  constructor MkOp
  special : a
  type    : IdlType
  name    : Maybe OperationName
  args    : ArgumentList

%runElab derive "Op" [Generic,Meta,Eq,Show]

public export
0 RegularOperation : Type
RegularOperation = Op ()

public export
0 SpecialOperation : Type
SpecialOperation = Op Special

public export
0 Operation : Type
Operation = Op (Maybe Special)

public export
regToOp : RegularOperation -> Operation
regToOp = record { special = Nothing }

public export
specToOp : SpecialOperation -> Operation
specToOp = record { special $= Just }

--------------------------------------------------------------------------------
--          Callbacks
--------------------------------------------------------------------------------


||| CallbackInterfaceMember ::
|||     Const
|||     RegularOperation
public export
0 CallbackInterfaceMember : Type
CallbackInterfaceMember = NS I [Const,RegularOperation]

||| CallbackInterfaceMembers ::
|||     ExtendedAttributeList CallbackInterfaceMember CallbackInterfaceMembers
|||     ε
public export
0 CallbackInterfaceMembers : Type
CallbackInterfaceMembers = List (Attributed CallbackInterfaceMember)


||| CallbackRest ::
|||     identifier = Type ( ArgumentList ) ;
public export
record CallbackRest where
  constructor MkCallbackRest
  name : Identifier
  type : IdlType
  args : ArgumentList

%runElab derive "CallbackRest" [Generic,Meta,Eq,Show]

--------------------------------------------------------------------------------
--          Dictionary
--------------------------------------------------------------------------------

||| Inheritance ::
|||     : identifier
|||     ε
public export
0 Inheritance : Type
Inheritance = Maybe Identifier

||| DictionaryMemberRest ::
|||     required TypeWithExtendedAttributes identifier ;
|||     Type identifier Default ;
public export
data DictionaryMemberRest : Type where
  Required :  (attrs : ExtAttributeList)
           -> (type : IdlType)
           -> (name : Identifier)
           -> DictionaryMemberRest

  Optional :  (type  : IdlType)
           -> (name  : Identifier)
           -> (deflt : Default)
           -> DictionaryMemberRest

%runElab derive "DictionaryMemberRest" [Generic,Meta,Eq,Show]

||| DictionaryMember ::
|||     ExtendedAttributeList DictionaryMemberRest
public export
0 DictionaryMember : Type
DictionaryMember = Attributed DictionaryMemberRest

||| DictionaryMembers ::
|||     DictionaryMember DictionaryMembers
|||     ε
public export
0 DictionaryMembers : Type
DictionaryMembers = List DictionaryMember
