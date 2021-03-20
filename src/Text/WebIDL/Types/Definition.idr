module Text.WebIDL.Types.Definition

import Generics.Derive
import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Argument
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
||| 
||| CallbackOrInterfaceOrMixin ::
|||     callback CallbackRestOrInterface
|||     interface InterfaceOrMixin
||| 
||| CallbackRestOrInterface ::
|||     CallbackRest
|||     interface identifier { CallbackInterfaceMembers } ;
|||
||| CallbackRest ::
|||     identifier = Type ( ArgumentList ) ;
||| 
||| InterfaceOrMixin ::
|||     InterfaceRest
|||     MixinRest
||| 
||| InterfaceRest ::
|||     identifier Inheritance { InterfaceMembers } ;
||| 
||| MixinRest ::
|||     mixin identifier { MixinMembers } ;
public export
data Definition : Type where
  Enum :  (name   : Identifier)
       -> (values : List1 StringLit)
       -> Definition

  Typedef :  (attributes : ExtAttributeList)
          -> (type       : IdlType)
          -> (name       : Identifier)
          -> Definition

  Interface :  (name : Identifier)
            -> (inherits : Inheritance)
            -> (members  : InterfaceMembers)
            -> Definition

  Dictionary :  (name : Identifier)
             -> (inherits : Inheritance)
             -> (members  : DictionaryMembers)
             -> Definition

  Mixin : (name : Identifier) -> (members  : MixinMembers) -> Definition

  Partial : (def : PartialDefinition) -> Definition

  Includes : (name : Identifier) -> (includes : Identifier) -> Definition

  Namespace :  (name : Identifier)
            -> (members : NamespaceMembers)
            -> Definition

  Callback :  (name : Identifier)
           -> (type : IdlType)
           -> (args : ArgumentList)
           -> Definition

  CallbackInterface :  (name : Identifier)
                    -> (members : CallbackInterfaceMembers)
                    -> Definition

%runElab derive "Definition" [Generic,Meta,Eq,Show]
