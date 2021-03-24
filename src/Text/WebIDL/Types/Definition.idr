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
  attributes : ExtAttributeList
  name       : Identifier
  type       : IdlType
  args       : ArgumentList

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
  attributes : ExtAttributeList
  name       : Identifier
  members    : CallbackInterfaceMembers

%runElab derive "CallbackInterface" [Generic,Meta,Eq,Show]

export
Types CallbackInterface where
  types = types . members

||| Dictionary ::
|||     dictionary identifier Inheritance { DictionaryMembers } ;
public export
record Dictionary where
  constructor MkDictionary
  attributes : ExtAttributeList
  name       : Identifier
  inherits   : Inheritance
  members    : DictionaryMembers

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
  attributes : ExtAttributeList
  name       : Identifier
  values     : List1 StringLit

%runElab derive "Enum" [Generic,Meta,Eq,Show]

export
Types Enum where
  types = const Nil

||| IncludesStatement ::
|||     identifier includes identifier ;
public export
record Includes where
  constructor MkIncludes
  attributes : ExtAttributeList
  name       : Identifier
  includes   : Identifier

%runElab derive "Includes" [Generic,Meta,Eq,Show]

export
Types Includes where
  types = const Nil

||| InterfaceRest ::
|||     identifier Inheritance { InterfaceMembers } ;
public export
record Interface where
  constructor MkInterface
  attributes : ExtAttributeList
  name       : Identifier
  inherits   : Inheritance
  members    : InterfaceMembers

%runElab derive "Interface" [Generic,Meta,Eq,Show]

export
Types Interface where
  types = types . members

||| MixinRest ::
|||     mixin identifier { MixinMembers } ;
public export
record Mixin where
  constructor MkMixin
  attributes : ExtAttributeList
  name       : Identifier
  members    : MixinMembers

%runElab derive "Mixin" [Generic,Meta,Eq,Show]

export
Types Mixin where
  types = types . members

||| Namespace ::
|||     namespace identifier { NamespaceMembers } ;
public export
record Namespace where
  constructor MkNamespace
  attributes : ExtAttributeList
  name       : Identifier
  members    : NamespaceMembers

%runElab derive "Namespace" [Generic,Meta,Eq,Show]

export
Types Namespace where
  types = types . members

||| Typedef ::
|||     typedef TypeWithExtendedAttributes identifier ;
public export
record Typedef where
  constructor MkTypedef
  attributes     : ExtAttributeList
  typeAttributes : ExtAttributeList
  type           : IdlType
  name           : Identifier

%runElab derive "Typedef" [Generic,Meta,Eq,Show]

export
Types Typedef where
  types = types . type

||| PartialDictionary ::
|||     dictionary identifier { DictionaryMembers } ;
public export
record PDictionary where
  constructor MkPDictionary
  attributes : ExtAttributeList
  name       : Identifier
  members    : DictionaryMembers

%runElab derive "PDictionary" [Generic,Meta,Eq,Show]

export
Types PDictionary where
  types = types . members

||| PartialInterfaceRest ::
|||     identifier { PartialInterfaceMembers } ;
public export
record PInterface where
  constructor MkPInterface
  attributes : ExtAttributeList
  name       : Identifier
  members    : PartialInterfaceMembers

%runElab derive "PInterface" [Generic,Meta,Eq,Show]

export
Types PInterface where
  types = types . members

||| MixinRest ::
|||     mixin identifier { MixinMembers } ;
public export
record PMixin where
  constructor MkPMixin
  attributes : ExtAttributeList
  name       : Identifier
  members    : MixinMembers

%runElab derive "PMixin" [Generic,Meta,Eq,Show]

export
Types PMixin where
  types = types . members

||| Namespace ::
|||     namespace identifier { NamespaceMembers } ;
public export
record PNamespace where
  constructor MkPNamespace
  attributes : ExtAttributeList
  name       : Identifier
  members    : NamespaceMembers

%runElab derive "PNamespace" [Generic,Meta,Eq,Show]

export
Types PNamespace where
  types = types . members

public export
DefTypes : List Type
DefTypes = [ Callback
           , CallbackInterface
           , Dictionary
           , Enum
           , Includes
           , Interface
           , Mixin
           , Namespace
           , Typedef
           ]

public export
PartTypes : List Type
PartTypes = [PDictionary, PInterface, PMixin, PNamespace]

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
public export
Definition : Type
Definition = NS I DefTypes

public export
0 Definitions : Type
Definitions = NP List DefTypes

||| PartialDefinition ::
|||     interface PartialInterfaceOrPartialMixin
|||     PartialDictionary
|||     Namespace
||| 
||| PartialInterfaceOrPartialMixin ::
|||     PartialInterfaceRest
|||     MixinRest
public export
Part : Type
Part = NS I PartTypes

public export
0 Parts : Type
Parts = NP List PartTypes

public export
accumNs : {ts : _} -> List (NS I ts) -> NP List ts
accumNs = foldl (\np,ns => hliftA2 (++) (toNP ns) np) hempty

public export
0 PartOrDef : Type
PartOrDef = NS I [Part,Definition]

public export
0 PartsAndDefs : Type
PartsAndDefs = NP List [Part,Definition]

public export
parts : PartsAndDefs -> Parts
parts = accumNs . get Part

public export
defs : PartsAndDefs -> Definitions
defs = accumNs . get Definition
