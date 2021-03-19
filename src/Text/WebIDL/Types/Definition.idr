module Text.WebIDL.Types.Definition

import Generics.Derive
import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Member
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Type

%language ElabReflection

namespace Partial
  ||| PartialDefinition ::
  |||     interface PartialInterfaceOrPartialMixin
  |||     PartialDictionary
  |||     Namespace
  ||| 
  ||| Namespace ::
  |||     namespace identifier { NamespaceMembers } ;
  ||| 
  ||| PartialDictionary ::
  |||     dictionary identifier { DictionaryMembers } ;
  ||| 
  ||| PartialInterfaceOrPartialMixin ::
  |||     PartialInterfaceRest
  |||     MixinRest
  ||| 
  ||| PartialInterfaceRest ::
  |||     identifier { PartialInterfaceMembers } ;
  ||| 
  ||| MixinRest ::
  |||     mixin identifier { MixinMembers } ;
  public export
  data PartialDefinition : Type where
    Dictionary :  (name : Identifier)
               -> (members  : DictionaryMembers)
               -> PartialDefinition

    Namespace :  (name : Identifier)
              -> (members : NamespaceMembers)
              -> PartialDefinition

    Mixin     :  (name : Identifier)
              -> (members : MixinMembers)
              -> PartialDefinition

    Interface :  (name : Identifier)
              -> (members : PartialInterfaceMembers)
              -> PartialDefinition

  %runElab derive "PartialDefinition" [Generic,Meta,Eq,Show]

||| Definition ::
|||     CallbackOrInterfaceOrMixin
|||     Namespace
|||     Partial
|||     Dictionary
|||     Enum
|||     Typedef
|||     IncludesStatement
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
||| 
||| Namespace ::
|||     namespace identifier { NamespaceMembers } ;
||| 
||| IncludesStatement ::
|||     identifier includes identifier ;
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

  Namespace :  (name : Identifier)
            -> (members : NamespaceMembers)
            -> Definition

  Partial : (def : PartialDefinition) -> Definition

  Includes : (name : Identifier) -> (includes : Identifier) -> Definition

%runElab derive "Definition" [Generic,Meta,Eq,Show]
