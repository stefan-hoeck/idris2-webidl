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

