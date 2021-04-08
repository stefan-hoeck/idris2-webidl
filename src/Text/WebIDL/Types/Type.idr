module Text.WebIDL.Types.Type

import Data.Bitraversable
import Data.Traversable
import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Identifier
import Generics.Derive

%language ElabReflection

||| BufferRelatedType ::
|||     ArrayBuffer
|||     DataView
|||     Int8Array
|||     Int16Array
|||     Int32Array
|||     Uint8Array
|||     Uint16Array
|||     Uint32Array
|||     Uint8ClampedArray
|||     Float32Array
|||     Float64Array
public export
data BufferRelatedType = ArrayBuffer
                       | DataView
                       | Int8Array
                       | Int16Array
                       | Int32Array
                       | Uint8Array
                       | Uint16Array
                       | Uint32Array
                       | Uint8ClampedArray
                       | Float32Array
                       | Float64Array

%runElab derive "BufferRelatedType" [Generic,Meta,Eq,Show]

||| StringType ::
|||     ByteString
|||     DOMString
|||     USVString
public export
data StringType = ByteString
                | DOMString
                | USVString

%runElab derive "Type.StringType" [Generic,Meta,Eq,Show]

public export
data IntType = Short | Long | LongLong

%runElab derive "Type.IntType" [Generic,Meta,Eq,Show]

public export
data FloatType = Float | Dbl

%runElab derive "Type.FloatType" [Generic,Meta,Eq,Show]

||| PrimitiveType ::
|||     UnsignedIntegerType
|||     UnrestrictedFloatType
|||     undefined
|||     boolean
|||     byte
|||     octet
|||     bigint
||| UnrestrictedFloatType ::
|||     unrestricted FloatType
|||     FloatType
||| 
||| FloatType ::
|||     float
|||     double
||| 
||| UnsignedIntegerType ::
|||     unsigned IntegerType
|||     IntegerType
||| 
||| IntegerType ::
|||     short
|||     long OptionalLong
||| 
||| OptionalLong ::
|||     long
|||     ε
public export
data PrimitiveType = Unsigned     IntType
                   | Signed       IntType
                   | Unrestricted FloatType
                   | Restricted   FloatType
                   | Undefined
                   | Boolean
                   | Byte
                   | Octet
                   | BigInt

%runElab derive "PrimitiveType" [Generic,Meta,Eq,Show]

public export
data ConstType = CP PrimitiveType | CI Identifier

%runElab derive "ConstType" [Generic,Meta,Eq,Show]

||| Null ::
|||     ?
|||     ε
public export
data Nullable a = MaybeNull a | NotNull a

%runElab derive "Nullable" [Generic,Meta,Eq,Show]

export
nullVal : Nullable a -> a
nullVal (MaybeNull x) = x
nullVal (NotNull x)   = x

mutual
  ||| Type ::
  |||     SingleType
  |||     UnionType Null
  |||
  ||| SingleType ::
  |||     DistinguishableType
  |||     any
  |||     PromiseType
  |||
  ||| PromiseType ::
  |||     Promise < Type >
  public export
  data IdlTypeF : (a : Type) -> (b : Type) -> Type where
    Any     : IdlTypeF a b
    D       : Nullable (DistinguishableF a b) -> IdlTypeF a b
    U       : Nullable (UnionTypeF a b) -> IdlTypeF a b
    Promise : IdlTypeF a b -> IdlTypeF a b

  ||| UnionType ::
  |||     ( UnionMemberType or UnionMemberType UnionMemberTypes )
  ||| 
  ||| UnionMemberTypes ::
  |||     or UnionMemberType UnionMemberTypes
  |||     ε
  public export
  record UnionTypeF (a : Type) (b : Type) where
    constructor UT
    fst  : UnionMemberTypeF a b
    snd  : UnionMemberTypeF a b
    rest : List (UnionMemberTypeF a b)

  ||| UnionMemberType ::
  |||     ExtendedAttributeList DistinguishableType
  |||     UnionType Null
  public export
  data UnionMemberTypeF : (a : Type) -> (b : Type) -> Type where
    UD : a -> Nullable (DistinguishableF a b) -> UnionMemberTypeF a b
    UU : Nullable (UnionTypeF a b) -> UnionMemberTypeF a b

  ||| DistinguishableType ::
  |||     PrimitiveType Null
  |||     StringType Null
  |||     identifier Null
  |||     sequence < TypeWithExtendedAttributes > Null
  |||     object Null
  |||     symbol Null
  |||     BufferRelatedType Null
  |||     FrozenArray < TypeWithExtendedAttributes > Null
  |||     ObservableArray < TypeWithExtendedAttributes > Null
  |||     RecordType Null
  |||
  ||| RecordType ::
  |||     record < StringType , TypeWithExtendedAttributes >
  public export
  data DistinguishableF : (a : Type) -> (b : Type) -> Type where
    P : PrimitiveType -> DistinguishableF a b
    S : StringType -> DistinguishableF a b
    I : b -> DistinguishableF a b
    B : BufferRelatedType -> DistinguishableF a b
    Sequence : a -> IdlTypeF a b -> DistinguishableF a b
    FrozenArray : a -> IdlTypeF a b -> DistinguishableF a b
    ObservableArray : a -> IdlTypeF a b -> DistinguishableF a b
    Record : StringType -> a -> IdlTypeF a b -> DistinguishableF a b
    Object : DistinguishableF a b
    Symbol : DistinguishableF a b

%runElab deriveMutual [ ("DistinguishableF", [Generic,Meta,Show,Eq])
                      , ("UnionMemberTypeF", [Generic,Meta,Show,Eq])
                      , ("UnionTypeF",       [Generic,Meta,Show,Eq])
                      , ("IdlTypeF",         [Generic,Meta,Show,Eq])
                      ]

public export
IdlType : Type
IdlType = IdlTypeF ExtAttributeList Identifier

public export
UnionType : Type
UnionType = UnionTypeF ExtAttributeList Identifier

public export
UnionMemberType : Type
UnionMemberType = UnionMemberTypeF ExtAttributeList Identifier

public export
Distinguishable : Type
Distinguishable = DistinguishableF ExtAttributeList Identifier

||| OptionalType ::
|||     , TypeWithExtendedAttributes
|||     ε
public export
0 OptionalType : Type
OptionalType = Maybe (Attributed IdlType)

||| Wraps and `Indentifier` as a non-nullable type.
export
identToType : Identifier -> IdlType
identToType = D . NotNull . I

||| The `Undefined` type
export
undefined : IdlType
undefined = D $ NotNull $ P Undefined

--------------------------------------------------------------------------------
--          Implementations
--------------------------------------------------------------------------------

mutual
  export
  Functor Nullable where map = mapDefault

  export
  Foldable Nullable where foldr = foldrDefault

  export
  Traversable Nullable where
    traverse f (MaybeNull x) = MaybeNull <$> f x
    traverse f (NotNull x)   = NotNull <$> f x
