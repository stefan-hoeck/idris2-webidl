module Text.WebIDL.Codegen.Types

import public Data.SortedMap
import public Data.Validated
import Text.WebIDL.Types

import Generics.Derive

%language ElabReflection

--------------------------------------------------------------------------------
--          Kinds
--------------------------------------------------------------------------------

||| The kind of a WebIDL identifier.
|||
||| This is needed when converting external types to Idris2 types
||| in the code generator: Aliases need to be resolved, Enums
||| converted to their `String` representation, Callback arguments
||| are Idris2 functions that need to be converted to an external
||| type.
public export
data Kind : Type where
  KAlias      : Identifier -> Kind
  KCallback   : Identifier -> Kind
  KDictionary : Identifier -> Kind
  KEnum       : Identifier -> Kind
  KInterface  : Identifier -> Kind
  KMixin      : Identifier -> Kind
  KOther      : Identifier -> Kind

%runElab derive "Kind" [Generic,Meta,Eq,Show]

public export
ident : Kind -> Identifier
ident (KAlias x)      = x
ident (KCallback x)   = x
ident (KDictionary x) = x
ident (KEnum x)       = x
ident (KInterface x)  = x
ident (KMixin x)      = x
ident (KOther x)      = x

public export
kindToString : Kind -> String
kindToString = value . ident

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

||| An idl type in the code generator pairs identifiers with
||| their `Kind`.
public export
CGType : Type
CGType = IdlTypeF ExtAttributeList Kind

public export
CGDist : Type
CGDist = DistinguishableF ExtAttributeList Kind

public export
CGUnion : Type
CGUnion = UnionTypeF ExtAttributeList Kind

public export
CGMember : Type
CGMember = UnionMemberTypeF ExtAttributeList Kind

||| A type paired with its unaliased from (if any).
||| In the unaliased form, all type aliases are fully resolved.
public export
record AType where
  constructor MkAType
  type  : CGType
  alias : Maybe CGType

||| True, if the type can be used as an index in a
||| WebIDL `Getter` or `Setter`, that is, it corresponds
||| to either an `unsigned long` or a `DOMString`.
export
isIndex : AType -> Bool
isIndex t = maybe (isIndex t.type) isIndex t.alias

||| A function's return type.
public export
data ReturnType : Type where
  ||| This will be mapped to `()` in the codegen.
  Undefined : ReturnType

  ||| The attribute in question might not be defined
  ||| The return type will be wrapped in `UndefOr` in
  ||| the code generator.
  UndefOr  : AType -> Maybe Default -> ReturnType

  ||| Nothing special about the wrapped return type.
  FromIdl   : AType -> ReturnType

||| Checks if the return type is `Undefined`.
public export
isUndefined : ReturnType -> Bool
isUndefined Undefined = True
isUndefined _         = False

||| Wrapps the given kind in an unaliased return type.
export
fromKind : Kind -> ReturnType
fromKind k = FromIdl $ MkAType (identToType k) Nothing

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

||| A function argument in the code generator.
public export
data CGArg : Type where
  ||| A mandatory function argument
  Mandatory : ArgumentName -> AType -> CGArg

  ||| An optional function argument together with its
  ||| default value if the argument is `undefined`.
  Optional  : ArgumentName -> AType -> Default -> CGArg

  ||| A variadic function argument
  VarArg    : ArgumentName -> AType -> CGArg

export
argName : CGArg -> ArgumentName
argName (Mandatory x _)  = x
argName (Optional x _ _) = x
argName (VarArg x _)     = x

export
argIdent : CGArg -> IdrisIdent
argIdent = fromString . value . argName

export
argType : CGArg -> AType
argType (Mandatory _ y)  = y
argType (Optional _ y _) = y
argType (VarArg _ y)     = y

export
isOptional : CGArg -> Bool
isOptional (Optional _ _ _) = True
isOptional _                = False

public export
Args : Type
Args = List CGArg

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

||| A function, for which we will generate some code.
public export
data CGFunction : Type where
  ||| An attribute setter.
  AttributeSet :  (name : AttributeName)
               -> (obj  : Kind)
               -> (tpe  : CGArg)
               -> CGFunction

  ||| An attribute getter.
  AttributeGet :  (name : AttributeName)
               -> (obj  : Kind)
               -> (tpe  : ReturnType)
               -> CGFunction

  ||| A static attribute setter.
  StaticAttributeSet :  (name : AttributeName)
                     -> (obj  : Kind)
                     -> (tpe  : CGArg)
                     -> CGFunction

  ||| A static attribute getter.
  StaticAttributeGet :  (name : AttributeName)
                     -> (obj  : Kind)
                     -> (tpe  : ReturnType)
                     -> CGFunction

  ||| An indexed getter.
  Getter : (obj : Kind) -> (index : CGArg) -> (tpe : ReturnType) -> CGFunction

  ||| An indexed setter.
  Setter : (obj : Kind) -> (index : CGArg) -> (value : CGArg) -> CGFunction

  ||| An interface constructor with (possibly) optional arguments.
  Constructor  :  (obj : Kind) -> (args : Args) -> CGFunction

  ||| An interface constructor with (possibly) optional arguments.
  DictConstructor : (obj : Kind) -> (args : Args) -> CGFunction

  ||| A regular function with (possibly) optional arguments.
  Regular      :  OperationName
               -> (obj : Kind)
               -> Args
               -> ReturnType
               -> CGFunction

  ||| A static function with (possibly) optional arguments.
  Static       :  OperationName
               -> (obj : Kind)
               -> Args
               -> ReturnType
               -> CGFunction

||| This is used for sorting lists of functions to
||| the determine the order in which they appear
||| in the generated code.
|||
||| Attributes will come first, sorted by name,
||| setters, getters, and unsetter grouped together in
||| that order.
|||
||| All other functions come later and will be sorted by name.
export
priority : CGFunction -> (Nat,String,Nat)
priority (DictConstructor n _)       = (0,value (ident n),0)
priority (Constructor n _)           = (0,value (ident n),0)
priority (StaticAttributeSet n _ _)  = (1,show n,1)
priority (StaticAttributeGet n _ _)  = (1,show n,0)
priority (Static n o _ _)            = (2,n.value ++ value (ident o),0)
priority (Getter _ _ _)              = (3,"",0)
priority (Setter _ _ _)              = (3,"",1)
priority (AttributeSet n _ _)        = (4,show n,1)
priority (AttributeGet n _ _)        = (4,show n,0)
priority (Regular n o _ _)           = (5,n.value ++ value (ident o),0)

--------------------------------------------------------------------------------
--          Inheritance
--------------------------------------------------------------------------------

||| An external, un-parameterized Javascript type, represented
||| by an identifier. Such a type comes with a parent
||| type (given as an `inheritance` value in the spec)
||| and a number of mixed in types.
|||
||| The actual name of the type is not included, as the set
||| of types is given in `Env` as a `SortedMap`.
public export
record JSType where
  constructor MkJSType
  parent : Maybe Identifier
  mixins : List Identifier

||| The parent types and mixins of a type. This is
||| used by the code generator to implement the
||| `JS.Inheritance.JSType` instances.
public export
record Supertypes where
  constructor MkSupertypes
  parents : List Identifier
  mixins  : List Identifier

--------------------------------------------------------------------------------
--          Domain
--------------------------------------------------------------------------------

public export
record CGDict where
  constructor MkDict
  name      : Identifier
  super     : Supertypes
  functions : List CGFunction

public export
record CGIface where
  constructor MkIface
  name      : Identifier
  super     : Supertypes
  constants : List Const
  functions : List CGFunction

public export
record CGMixin where
  constructor MkMixin
  name      : Identifier
  constants : List Const
  functions : List CGFunction

public export
record CGCallback where
  constructor MkCallback
  name      : Identifier
  constants : List Const
  type      : IdlType
  args      : ArgumentList

public export
record CGTypedef where
  constructor MkTypedef
  name      : Identifier
  type      : CGType

public export
record CGDomain where
  constructor MkDomain
  name      : String
  callbacks : List CGCallback
  dicts     : List CGDict
  enums     : List Enum
  ifaces    : List CGIface
  mixins    : List CGMixin
  typedefs  : List CGTypedef

export
domainFunctions : CGDomain -> List CGFunction
domainFunctions d =  (d.dicts  >>= functions)
                  ++ (d.ifaces >>= functions)
                  ++ (d.mixins >>= functions)

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

||| Codegen environment.
|||
||| This includes a mapping from identifiers to the kinds
||| they represent, a mapping from identifiers to their
||| inheritance relations (`jsTypes`) and a mapping of
||| type aliases.
|||
||| The `maxInheritance` constant is used when calculating a
||| type's supertypes to avoid a potentially infinite loop.
public export
record Env where
  constructor MkEnv
  maxInheritance : Nat
  kinds          : SortedMap Identifier Kind
  jsTypes        : SortedMap Identifier JSType
  aliases        : SortedMap Identifier CGType

--------------------------------------------------------------------------------
--          Codegen Errors
--------------------------------------------------------------------------------

public export
data CodegenErr : Type where
  CBInterfaceInvalidOps  : Domain -> Identifier -> Nat -> CodegenErr
  InvalidGetter          : Domain -> Identifier -> CodegenErr
  InvalidSetter          : Domain -> Identifier -> CodegenErr
  RegularOpWithoutName   : Domain -> Identifier -> CodegenErr

public export
Codegen : Type -> Type
Codegen = Either (List CodegenErr)

public export
CodegenV : Type -> Type
CodegenV = Validated (List CodegenErr)
