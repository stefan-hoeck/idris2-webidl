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
