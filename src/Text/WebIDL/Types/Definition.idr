module Text.WebIDL.Types.Definition

import Generics.Derive
import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Argument
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Member
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Type

%hide Language.Reflection.TT.Namespace
%language ElabReflection

||| CallbackRest ::
|||     identifier = Type ( ArgumentList ) ;
public export
record Callback where
  constructor MkCallback
  name : Identifier
  type : IdlType
  args : ArgumentList

%runElab derive "Callback" [Generic,Meta,Eq,Show]

export
Types Callback where
  types c = types c.type ++ types c.args

||| CallbackRestOrInterface ::
|||     CallbackRest
|||     interface identifier { CallbackInterfaceMembers } ;
public export
record CallbackInterface where
  constructor MkCallbackInterface
  name    : Identifier
  members : CallbackInterfaceMembers

%runElab derive "CallbackInterface" [Generic,Meta,Eq,Show]

export
Types CallbackInterface where
  types = types . members

||| Dictionary ::
|||     dictionary identifier Inheritance { DictionaryMembers } ;
public export
record Dictionary where
  constructor MkDictionary
  name     : Identifier
  inherits : Inheritance
  members  : DictionaryMembers

%runElab derive "Dictionary" [Generic,Meta,Eq,Show]

export
Types Dictionary where
  types = types . members

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
public export
record Enum where
  constructor MkEnum
  name   : Identifier
  values : List1 StringLit

%runElab derive "Enum" [Generic,Meta,Eq,Show]

||| IncludesStatement ::
|||     identifier includes identifier ;
public export
record Includes where
  constructor MkIncludes
  name     : Identifier
  includes : Identifier

%runElab derive "Includes" [Generic,Meta,Eq,Show]

||| InterfaceRest ::
|||     identifier Inheritance { InterfaceMembers } ;
public export
record Interface where
  constructor MkInterface
  name     : Identifier
  inherits : Inheritance
  members  : InterfaceMembers

%runElab derive "Interface" [Generic,Meta,Eq,Show]

export
Types Interface where
  types = types . members

||| MixinRest ::
|||     mixin identifier { MixinMembers } ;
public export
record Mixin where
  constructor MkMixin
  name    : Identifier
  members : MixinMembers

%runElab derive "Mixin" [Generic,Meta,Eq,Show]

export
Types Mixin where
  types = types . members

||| Namespace ::
|||     namespace identifier { NamespaceMembers } ;
public export
record Namespace where
  constructor MkNamespace
  name    : Identifier
  members : NamespaceMembers

%runElab derive "Namespace" [Generic,Meta,Eq,Show]

export
Types Namespace where
  types = types . members

||| Typedef ::
|||     typedef TypeWithExtendedAttributes identifier ;
public export
record Typedef where
  constructor MkTypedef
  attributes : ExtAttributeList
  type       : IdlType
  name       : Identifier

%runElab derive "Typedef" [Generic,Meta,Eq,Show]

export
Types Typedef where
  types = types . type

||| PartialDictionary ::
|||     dictionary identifier { DictionaryMembers } ;
public export
record PDictionary where
  constructor MkPDictionary
  name    : Identifier
  members : DictionaryMembers

%runElab derive "PDictionary" [Generic,Meta,Eq,Show]

export
Types PDictionary where
  types = types . members

||| PartialInterfaceRest ::
|||     identifier { PartialInterfaceMembers } ;
public export
record PInterface where
  constructor MkPInterface
  name    : Identifier
  members : PartialInterfaceMembers

%runElab derive "PInterface" [Generic,Meta,Eq,Show]

export
Types PInterface where
  types = types . members

||| MixinRest ::
|||     mixin identifier { MixinMembers } ;
public export
record PMixin where
  constructor MkPMixin
  name    : Identifier
  members : MixinMembers

%runElab derive "PMixin" [Generic,Meta,Eq,Show]

export
Types PMixin where
  types = types . members

||| Namespace ::
|||     namespace identifier { NamespaceMembers } ;
public export
record PNamespace where
  constructor MkPNamespace
  name    : Identifier
  members : NamespaceMembers

%runElab derive "PNamespace" [Generic,Meta,Eq,Show]

export
Types PNamespace where
  types = types . members

||| Definition ::
|||     CallbackOrInterfaceOrMixin
|||     Namespace
|||     Partial
|||     Dictionary
|||     Enum
|||     Typedef
|||     IncludesStatement
||| CallbackOrInterfaceOrMixin ::
|||     callback CallbackRestOrInterface
|||     interface InterfaceOrMixin
||| 
||| InterfaceOrMixin ::
|||     InterfaceRest
|||     MixinRest
|||
||| PartialDefinition ::
|||     interface PartialInterfaceOrPartialMixin
|||     PartialDictionary
|||     Namespace
||| 
||| PartialInterfaceOrPartialMixin ::
|||     PartialInterfaceRest
|||     MixinRest
||| 
public export
0 Definition : Type
Definition = NS I [ Callback
                  , CallbackInterface
                  , Dictionary
                  , Enum
                  , Includes
                  , Interface
                  , Mixin
                  , Namespace
                  , Typedef
                  , PDictionary
                  , PInterface
                  , PMixin
                  , PNamespace
                  ]

public export
record Definitions where
  constructor MkDefinitions
  callbackInterfaces  : List (Attributed CallbackInterface)
  callbacks           : List (Attributed Callback)
  dictionaries        : List (Attributed Dictionary)
  enums               : List (Attributed Enum)
  includeStatements   : List (Attributed Includes)
  interfaces          : List (Attributed Interface)
  mixins              : List (Attributed Mixin)
  namespaces          : List (Attributed Namespace)
  partialDictionaries : List (Attributed PDictionary)
  partialInterfaces   : List (Attributed PInterface)
  partialMixins       : List (Attributed PMixin)
  partialNamespaces   : List (Attributed PNamespace)
  typedefs            : List (Attributed Typedef)

%runElab derive "Definitions" [Generic,Meta,Eq,Show,Semigroup,Monoid]

export
Types Definitions where
  types d =  types d.callbackInterfaces
          ++ types d.callbacks
          ++ types d.dictionaries
          ++ types d.interfaces
          ++ types d.mixins
          ++ types d.namespaces
          ++ types d.partialDictionaries
          ++ types d.partialInterfaces
          ++ types d.partialMixins
          ++ types d.partialNamespaces
          ++ types d.typedefs

export
toDefinitions : Attributed Definition -> Definitions
toDefinitions (a,d) =
  collapseNS $ hliftA2 injDefn [ \d => record {callbacks = d} neutral
                               , \d => record {callbackInterfaces = d} neutral
                               , \d => record {dictionaries = d} neutral
                               , \d => record {enums = d} neutral
                               , \d => record {includeStatements = d} neutral
                               , \d => record {interfaces = d} neutral
                               , \d => record {mixins = d} neutral
                               , \d => record {namespaces = d} neutral
                               , \d => record {typedefs = d} neutral
                               , \d => record {partialDictionaries = d} neutral
                               , \d => record {partialInterfaces = d} neutral
                               , \d => record {partialMixins = d} neutral
                               , \d => record {partialNamespaces = d} neutral
                               ] d

  where injDefn :  forall a . (List $ Attributed a -> Definitions)
                -> a -> Definitions
        injDefn f x = f [(a, x)]
