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
data ConstTypeF a = CP PrimitiveType | CI a

%runElab derive "ConstTypeF" [Generic,Meta,Eq,Show]

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

public export
ConstType : Type
ConstType = ConstTypeF Identifier

||| OptionalType ::
|||     , TypeWithExtendedAttributes
|||     ε
public export
0 OptionalType : Type
OptionalType = Maybe (Attributed IdlType)

||| Wraps and `Indentifier` as a non-nullable type.
export
identToType : b -> IdlTypeF a b
identToType = D . NotNull . I

||| The `Undefined` type
export
undefined : IdlTypeF a b
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

mutual
  export
  Functor ConstTypeF where map = mapDefault

  export
  Foldable ConstTypeF where foldr = foldrDefault

  export
  Traversable ConstTypeF where
    traverse _ (CP x) = pure (CP x)
    traverse f (CI x) = CI <$> f x

mutual
  export
  Bifunctor DistinguishableF where bimap = bimapDefault

  export
  Bifoldable DistinguishableF where bifoldr = bifoldrDefault

  export
  Bitraversable DistinguishableF where
    bitraverse _ _ (P x) = pure (P x)
    bitraverse _ _ (S x) = pure (S x)
    bitraverse _ g (I x) = I <$> g x
    bitraverse _ _ (B x) = pure (B x)
    bitraverse f g (Sequence x y) = [| Sequence (f x) (bitraverse f g y) |]
    bitraverse f g (FrozenArray x y) = [| FrozenArray (f x) (bitraverse f g y) |]
    bitraverse f g (ObservableArray x y) = [| ObservableArray (f x) (bitraverse f g y) |]
    bitraverse f g (Record x y z) = [| Record (pure x) (f y) (bitraverse f g z) |]
    bitraverse _ _ Object = pure Object
    bitraverse _ _ Symbol = pure Symbol

  export
  Functor (DistinguishableF a) where map = bimap id

  export
  Foldable (DistinguishableF a) where foldr = bifoldr (const id)

  export
  Traversable (DistinguishableF a) where traverse = bitraverse pure

  export
  Bifunctor UnionMemberTypeF where bimap = bimapDefault

  export
  Bifoldable UnionMemberTypeF where bifoldr = bifoldrDefault

  export
  Bitraversable UnionMemberTypeF where
    bitraverse f g (UD x y) = [| UD (f x) (traverse (bitraverse f g) y) |]
    bitraverse f g (UU x) = UU <$> traverse (bitraverse f g) x

  export
  Functor (UnionMemberTypeF a) where map = bimap id

  export
  Foldable (UnionMemberTypeF a) where foldr = bifoldr (const id)

  export
  Traversable (UnionMemberTypeF a) where traverse = bitraverse pure

  export
  Bifunctor UnionTypeF where bimap = bimapDefault

  export
  Bifoldable UnionTypeF where bifoldr = bifoldrDefault

  export
  Bitraversable UnionTypeF where
    bitraverse f g (UT a b ts) =
      [| UT (bitraverse f g a) (bitraverse f g b)
            (traverse (bitraverse f g) ts) |]

  export
  Functor (UnionTypeF a) where map = bimap id

  export
  Foldable (UnionTypeF a) where foldr = bifoldr (const id)

  export
  Traversable (UnionTypeF a) where traverse = bitraverse pure

  export
  Bifunctor IdlTypeF where bimap = bimapDefault

  export
  Bifoldable IdlTypeF where bifoldr = bifoldrDefault

  export
  Bitraversable IdlTypeF where
    bitraverse f g Any = pure Any
    bitraverse f g (D x) = D <$> traverse (bitraverse f g) x
    bitraverse f g (U x) = U <$> traverse (bitraverse f g) x
    bitraverse f g (Promise x) = Promise <$> bitraverse f g x

  export
  Functor (IdlTypeF a) where map = bimap id

  export
  Foldable (IdlTypeF a) where foldr = bifoldr (const id)

  export
  Traversable (IdlTypeF a) where traverse = bitraverse pure
