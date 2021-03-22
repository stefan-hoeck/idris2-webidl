module Text.WebIDL.Types.Type

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
  data IdlType = Any
               | D (Nullable Distinguishable)
               | U (Nullable UnionType)
               | Promise IdlType

  export
  sizeIdl : IdlType -> Nat
  sizeIdl Any         = 1
  sizeIdl (D x)       = 1 + sizeDT (nullVal x)
  sizeIdl (U x)       = 1 + sizeUT (nullVal x)
  sizeIdl (Promise x) = 1 + sizeIdl x

  ||| UnionType ::
  |||     ( UnionMemberType or UnionMemberType UnionMemberTypes )
  ||| 
  ||| UnionMemberTypes ::
  |||     or UnionMemberType UnionMemberTypes
  |||     ε
  public export
  record UnionType where
    constructor UT
    fst  : UnionMemberType
    snd  : UnionMemberType
    rest : List UnionMemberType

  sizeUT : UnionType -> Nat
  sizeUT (UT fst snd rest) = 1 + foldl (\n,t => n + sizeUMT t)
                                       (sizeUMT fst + sizeUMT snd)
                                       rest

  ||| UnionMemberType ::
  |||     ExtendedAttributeList DistinguishableType
  |||     UnionType Null
  public export
  data UnionMemberType =
      UD (Attributed $ Nullable Distinguishable)
    | UU (Nullable UnionType)

  sizeUMT : UnionMemberType -> Nat
  sizeUMT (UD (x, y)) = 1 + sizeDT (nullVal y)
  sizeUMT (UU x)      = 1 + sizeUT (nullVal x)

  
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
  data Distinguishable =
      P PrimitiveType
    | S StringType
    | I Identifier
    | B BufferRelatedType
    | Sequence (Attributed IdlType)
    | FrozenArray (Attributed IdlType)
    | ObservableArray (Attributed IdlType)
    | Record StringType (Attributed IdlType)
    | Object
    | Symbol

  sizeDT : Distinguishable -> Nat
  sizeDT (P x) = 1
  sizeDT (S x) = 1
  sizeDT (I x) = 1
  sizeDT (B x) = 1
  sizeDT (Sequence x) = 1 + sizeIdl (snd x)
  sizeDT (FrozenArray x) = 1 + sizeIdl (snd x)
  sizeDT (ObservableArray x) = 1 + sizeIdl (snd x)
  sizeDT (Record x y) = 2 + sizeIdl (snd y)
  sizeDT Object = 1
  sizeDT Symbol = 1

%runElab deriveMutual [ ("Distinguishable", [Generic,Meta,Show,Eq])
                      , ("UnionMemberType", [Generic,Meta,Show,Eq])
                      , ("UnionType",       [Generic,Meta,Show,Eq])
                      , ("IdlType",         [Generic,Meta,Show,Eq])
                      ]

||| OptionalType ::
|||     , TypeWithExtendedAttributes
|||     ε
public export
0 OptionalType : Type
OptionalType = Maybe (Attributed IdlType)

--------------------------------------------------------------------------------
--          Types Interface
--------------------------------------------------------------------------------

public export
interface Types a where
  types : a -> List IdlType

export
Types () where
  types () = []

export
Types IdlType where
  types t = [t]

export
Types ConstType where
  types (CP x) = types (D . NotNull $ P x)
  types (CI x) = types (D . NotNull $ I x)

export
Types a => Types (List a) where
  types = (>>= types)

export
Types a => Types (Maybe a) where
  types = maybe Nil types

export
Types a => Types (Attributed a) where
  types (_,a) = types a

export
NP Types ts => Types (NS I ts) where
  types ns = collapseNS $ hcmap Types types ns
