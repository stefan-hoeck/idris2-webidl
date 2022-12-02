module Text.WebIDL.Codegen.Types

import Derive.Prelude
import public Data.SortedMap
import public Data.Validated
import Text.WebIDL.Types

%hide Derive.Enum.Enum
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
  KInterface  : (isParent : Bool) -> Identifier -> Kind
  KMixin      : Identifier -> Kind
  KOther      : Identifier -> Kind

%runElab derive "Kind" [Eq,Show]

public export
isParent : Kind -> Bool
isParent (KDictionary _)  = True
isParent (KInterface b _) = b
isParent (KMixin _)       = True
isParent _                = False

public export
ident : Kind -> Identifier
ident (KAlias x)        = x
ident (KCallback x)     = x
ident (KDictionary x)   = x
ident (KEnum x)         = x
ident (KInterface _ x)  = x
ident (KMixin x)        = x
ident (KOther x)        = x

public export
kindToString : Kind -> String
kindToString = value . ident

public export
data Wrapper = Direct | Opt | May | OptMay

public export
opt : Wrapper -> Wrapper
opt Direct = Opt
opt May    = OptMay
opt i      = i

public export
may : Wrapper -> Wrapper
may Direct = May
may i      = i

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

mutual
  public export
  data SimpleType : Type where
    ||| The undefined type (or () if it is a return type)
    Undef        : SimpleType

    ||| `Boolean` at the FFI, `Bool` in the API.
    Boolean      : SimpleType

    ||| A Web IDL interface. This has an instance of `SafeCast`,
    ||| and is being abstracted over when used in an argument
    ||| list in the API, but only, when the `isParent` flag is set to true.
    ||| If this flag is `False`, meaning that there are no subtypes of
    ||| this type, we do not abstract over the type to improve
    ||| type inference.
    Interface   : (isParent : Bool) -> Identifier  -> SimpleType

    ||| A dictionary, specifying a Javascript object with a
    ||| set of mandatory and optional attributes.
    ||| This is always abstracted over, since theoretically
    ||| every value with the same set of attributes is
    ||| a dictionary of the given type.
    |||
    ||| The type of a dictionary cannot be verified at
    ||| runtime, therefore they have no instance of `SafeCast`.
    Dictionary  : Identifier  -> SimpleType

    ||| A Web IDL `Mixin` is a set of attributes and operations (functions)
    ||| shared by several types. A type includes a given mixin, if
    ||| a corresponding `includes` statement is provided in the spec.
    |||
    ||| Mixins do not define new types, and whether a value implements
    ||| a given mixin can typcally not be verified at runtime, therefore
    ||| mixins come without an instance of `SafeCast`.
    ||| runtime, therefore they have no instance of `SafeCast`.
    Mixin  : Identifier  -> SimpleType

    ||| Primitive type or a wrapper of a primitive.
    ||| This is the same at the FFI and API and
    ||| has an instance of `SafeCast`.
    Primitive : String      -> SimpleType

    ||| Types that do not change between FFI and API
    Unchangeable : String      -> SimpleType

    ||| Enum type at the API, Strings at the FFI
    Enum         : Identifier  -> SimpleType

    ||| Some kind of Array
    Array        : CGType      -> SimpleType

    ||| Some kind of Record
    Record       : String      -> CGType -> SimpleType

  public export
  data CGType : Type where
    Any     : CGType
    Promise : CGType                      -> CGType
    Simple  : Nullable SimpleType         -> CGType
    Union   : Nullable (List1 SimpleType) -> CGType

||| True, if the type can be used as an index in a
||| WebIDL `Getter` or `Setter`, that is, it corresponds
||| to either an `unsigned long` or a `DOMString`.
export
isIndex : CGType -> Bool
isIndex (Simple $ NotNull $ Primitive "String") = True
isIndex (Simple $ NotNull $ Primitive "Bits32") = True
isIndex _                                          = False

namespace SimpleType
  ||| True, if the FFI representation of the given type
  ||| has a `SafeCast` implementation
  export
  safeCast : SimpleType -> Bool
  safeCast Undef            = True
  safeCast Boolean          = True
  safeCast (Interface _ x)  = True
  safeCast (Dictionary _)   = False
  safeCast (Mixin _)        = False
  safeCast (Primitive x)    = True
  safeCast (Unchangeable x) = False
  safeCast (Enum x)         = True
  safeCast (Array x)        = False
  safeCast (Record x y)     = False

  ||| True, if the type uses the same representation in
  ||| the FFI and the API as a function argument
  export
  sameArgType : SimpleType -> Bool
  sameArgType Undef            = True
  sameArgType Boolean          = False
  sameArgType (Interface b _)  = not b
  sameArgType (Dictionary _)   = False
  sameArgType (Mixin _)        = False
  sameArgType (Primitive t)    = True
  sameArgType (Unchangeable _) = True
  sameArgType (Enum _)         = False
  sameArgType (Array _)        = True
  sameArgType (Record _ _)     = True

  ||| True, if the type uses the same representation in
  ||| the FFI and the API as a return value.
  export
  sameRetType : SimpleType -> Bool
  sameRetType Undef            = True
  sameRetType Boolean          = False
  sameRetType (Interface _ _)  = True
  sameRetType (Dictionary _)   = True
  sameRetType (Mixin _)        = True
  sameRetType (Primitive _)    = True
  sameRetType (Unchangeable _) = True
  sameRetType (Enum _)         = False
  sameRetType (Array _)        = True
  sameRetType (Record _ _)     = True

  export
  inheritance : SimpleType -> Maybe (Identifier,Wrapper)
  inheritance (Interface True x)  = Just (x,Direct)
  inheritance (Dictionary x)      = Just (x,Direct)
  inheritance (Mixin x)           = Just (x,Direct)
  inheritance _                   = Nothing

namespace CGType

  public export
  simple : SimpleType -> CGType
  simple = Simple . NotNull

  public export
  unchangeable : String -> CGType
  unchangeable = simple . Unchangeable

  public export
  iface : Bool -> Identifier -> CGType
  iface b = simple . Interface b

  public export
  mixin : Identifier -> CGType
  mixin = simple . Mixin

  public export
  dict : Identifier -> CGType
  dict = simple . Dictionary

  ||| Wrapps the given kind in return type.
  export
  fromKind : Kind -> CGType
  fromKind (KAlias x)        = unchangeable x.value
  fromKind (KCallback x)     = unchangeable x.value
  fromKind (KDictionary x)   = dict x
  fromKind (KEnum x)         = Simple . NotNull $ Enum x
  fromKind (KInterface b x)  = iface b x
  fromKind (KMixin x)        = mixin x
  fromKind (KOther x)        = unchangeable x.value

  ||| True, if the FFI representation of the given type
  ||| has a `SafeCast` implementation
  export
  safeCast : CGType -> Bool
  safeCast Any         = True
  safeCast (Promise x) = False
  safeCast (Simple x)  = safeCast $ nullVal x
  safeCast (Union _)   = False

  ||| True, if the given type is the same in the API and
  ||| the FFI when used as an argument
  export
  sameArgType : CGType -> Bool
  sameArgType Any                    = False
  sameArgType (Promise x)            = True
  sameArgType (Simple $ MaybeNull _) = False
  sameArgType (Simple $ NotNull x)   = sameArgType x
  sameArgType (Union  _)             = False

  ||| True, if the given type is the same in the API and
  ||| the FFI when used as a return value
  export
  sameRetType : CGType -> Bool
  sameRetType Any                    = False
  sameRetType (Promise x)            = True
  sameRetType (Simple $ MaybeNull _) = False
  sameRetType (Simple $ NotNull x)   = sameRetType x
  sameRetType (Union  $ MaybeNull _) = False
  sameRetType (Union  $ NotNull xs)  = not $ all safeCast xs

  export
  inheritance : CGType -> Maybe (Identifier,Wrapper)
  inheritance (Simple $ MaybeNull x) = map may <$> inheritance x
  inheritance (Simple $ NotNull x)   = inheritance x
  inheritance _                      = Nothing

--------------------------------------------------------------------------------
--          ReturnType
--------------------------------------------------------------------------------

||| A function's return type.
public export
data ReturnType : Type where
  ||| This will be mapped to `()` in the codegen.
  Undefined : ReturnType

  ||| The attribute in question might not be defined
  ||| The return type will be wrapped in `UndefOr` in
  ||| the code generator.
  UndefOr  : CGType -> Maybe Default -> ReturnType

  ||| Nothing special about the wrapped return type.
  Def      : CGType -> ReturnType

||| Checks if the return type is `Undefined`.
public export
isUndefined : ReturnType -> Bool
isUndefined Undefined = True
isUndefined _         = False

namespace ReturnType

  public export
  simple : SimpleType -> ReturnType
  simple = Def . simple

  public export
  unchangeable : String -> ReturnType
  unchangeable = Def . unchangeable

  ||| Wrapps the given kind in return type.
  export
  fromKind : Kind -> ReturnType
  fromKind = Def . CGType.fromKind

  ||| True, if the given FFI type has a `SafeCast` instance
  export
  safeCast : ReturnType -> Bool
  safeCast Undefined     = True
  safeCast (UndefOr x _) = safeCast x
  safeCast (Def x)       = safeCast x

  ||| True, if the given return type is the same in the API and
  ||| the FFI.
  export
  sameType : ReturnType -> Bool
  sameType Undefined     = True
  sameType (UndefOr _ _) = False
  sameType (Def x)       = sameRetType x

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

public export
record CGConstType where
  constructor MkConstType
  primitive : String

public export
record CGConst where
  constructor MkConst
  type  : CGConstType
  name  : Identifier
  value : ConstValue

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

||| A function argument in the code generator.
public export
data CGArg : Type where
  ||| A mandatory function argument
  Mandatory : ArgumentName -> CGType -> CGArg

  ||| An optional function argument together with its
  ||| default value if the argument is `undefined`.
  Optional  : ArgumentName -> CGType -> Default -> CGArg

  ||| A variadic function argument
  VarArg    : ArgumentName -> CGType -> CGArg

export
argName : CGArg -> ArgumentName
argName (Mandatory x _)  = x
argName (Optional x _ _) = x
argName (VarArg x _)     = x

export
argType : CGArg -> CGType
argType (Mandatory _ y)  = y
argType (Optional _ y _) = y
argType (VarArg _ y)     = y

export
isOptional : CGArg -> Bool
isOptional (Optional _ _ _) = True
isOptional _                = False

namespace CGArg

  ||| True, if the given FFI type has a `SafeCast` instance
  export
  safeCast : CGArg -> Bool
  safeCast (Mandatory _ t)  = safeCast t
  safeCast (Optional _ t _) = safeCast t
  safeCast (VarArg _ _)     = False

  ||| True, if the given argument type is the same in the API and
  ||| the FFI.
  export
  sameType : CGArg -> Bool
  sameType (Mandatory _ t)  = sameArgType t
  sameType (Optional _ _ _) = False
  sameType (VarArg _ _)     = False

  export
  inheritance : CGArg -> Maybe (Identifier,Wrapper)
  inheritance (Mandatory _ t)  = inheritance t
  inheritance (Optional _ t _) = map opt <$> inheritance t
  inheritance (VarArg _ _)     = Nothing

public export
Args : Type
Args = List CGArg

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

||| A function, for which we will generate some code.
public export
data CGFunction : Type where
  ||| A read-write attribute
  Attribute :  (name : AttributeName)
            -> (obj  : Kind)
            -> (tpe  : CGArg)
            -> (ret  : ReturnType)
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
priority (Attribute n _ _ _)         = (4,show n,0)
priority (AttributeGet n _ _)        = (4,show n,1)
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
  constants : List CGConst
  functions : List CGFunction

public export
record CGMixin where
  constructor MkMixin
  name      : Identifier
  constants : List CGConst
  functions : List CGFunction

public export
record CGCallback where
  constructor MkCallback
  name      : Identifier
  constants : List CGConst
  type      : ReturnType
  args      : Args

public export
record CGDomain where
  constructor MkDomain
  name      : String
  callbacks : List CGCallback
  dicts     : List CGDict
  enums     : List Enum
  ifaces    : List CGIface
  mixins    : List CGMixin

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
  aliases        : SortedMap Identifier (IdlTypeF ExtAttributeList Kind)

--------------------------------------------------------------------------------
--          Codegen Errors
--------------------------------------------------------------------------------

public export
data CodegenErr : Type where
  AnyInUnion             : Domain -> CodegenErr
  CBInterfaceInvalidOps  : Domain -> Identifier -> Nat -> CodegenErr
  InvalidConstType       : Domain -> CodegenErr
  InvalidGetter          : Domain -> Identifier -> CodegenErr
  InvalidSetter          : Domain -> Identifier -> CodegenErr
  NullableAny            : Domain -> CodegenErr
  NullablePromise        : Domain -> CodegenErr
  PromiseInUnion         : Domain -> CodegenErr
  RegularOpWithoutName   : Domain -> Identifier -> CodegenErr
  UnresolvedAlias        : Domain -> Identifier -> CodegenErr

public export
Codegen : Type -> Type
Codegen = Either (List CodegenErr)

public export
CodegenV : Type -> Type
CodegenV = Validated (List CodegenErr)
