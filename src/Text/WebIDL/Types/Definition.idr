module Text.WebIDL.Types.Definition

import Generics.Derive
import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Member
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Type

%language ElabReflection

||| Enum ::
|||     enum identifier { EnumValueList } ;
||| 
||| EnumValueList ::
|||     string EnumValueListComma
||| 
||| EnumValueListComma ::
|||     , EnumValueListString
|||     ε
||| 
||| EnumValueListString ::
|||     string EnumValueListComma
|||     ε
|||
||| Typedef ::
|||     typedef TypeWithExtendedAttributes identifier ;
|||
||| Dictionary ::
|||     dictionary identifier Inheritance { DictionaryMembers } ;
public export
data Definition : Type where
  Enum :  (name   : Identifier)
       -> (values : List1 StringLit)
       -> Definition

  Typedef :  (attributes : ExtAttributeList)
          -> (type       : IdlType)
          -> (name       : Identifier)
          -> Definition

  Dictionary :  (name : Identifier)
             -> (inherits : Inheritance)
             -> (members  : DictionaryMembers)
             -> Definition

%runElab derive "Definition" [Generic,Meta,Eq,Show]
