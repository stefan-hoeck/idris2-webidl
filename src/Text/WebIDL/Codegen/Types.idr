module Text.WebIDL.Codegen.Types

import Language.Reflection.Refined
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types

prettyIdent : Identifier -> Doc ann
prettyIdent (MkIdent "void") = "Undefined"
prettyIdent (MkIdent value) = pretty value

export
Pretty BufferRelatedType where
  pretty Uint8Array        = "UInt8Array"
  pretty Uint16Array       = "UInt8Array"
  pretty Uint32Array       = "UInt8Array"
  pretty Uint8ClampedArray = "UInt8ClampedArray"
  pretty x                 = pretty $ show x

export
Pretty PrimitiveType where
  pretty (Unsigned Short)    = "UInt16"
  pretty (Unsigned Long)     = "UInt32"
  pretty (Unsigned LongLong) = "UInt64"
  pretty (Signed Short)      = "Int16"
  pretty (Signed Long)       = "Int32"
  pretty (Signed LongLong)   = "Int64"
  pretty (Unrestricted x)    = "Double"
  pretty (Restricted x)      = "Double"
  pretty Undefined           = "Undefined"
  pretty Boolean             = "Boolean"
  pretty Byte                = "Int8"
  pretty Octet               = "UInt8"
  pretty BigInt              = "Integer"

export
Pretty StringType where
  pretty ByteString = "ByteString"
  pretty DOMString  = "String"
  pretty USVString  = "String"

export
Pretty a => Pretty (Nullable a) where
  prettyPrec p (MaybeNull x) = prettyCon p "Nullable" [prettyPrec App x]
  prettyPrec p (NotNull x)   = prettyPrec p x

mutual
  export
  Pretty IdlType where
    prettyPrec _ Any         = "AnyPtr"
    prettyPrec p (D x)       = prettyPrec p x
    prettyPrec p (U x)       = prettyPrec p x
    prettyPrec p (Promise x) = prettyCon p "Promise" [prettyPrec App x]

  export
  Pretty Distinguishable where
    prettyPrec p (P x) = prettyPrec p x
    prettyPrec p (S x) = prettyPrec p x
    prettyPrec p (I x) = prettyIdent x
    prettyPrec p (B x) = prettyPrec p x
    prettyPrec p (Sequence (_,x)) =
      prettyCon p "Array" [prettyPrec App x]
    prettyPrec p (FrozenArray (_,x)) =
      prettyCon p "Array" [prettyPrec App x]
    prettyPrec p (ObservableArray (_,x)) =
      prettyCon p "Array" [prettyPrec App x]
    prettyPrec p Object = "Object"
    prettyPrec p Symbol = "Symbol"
    prettyPrec p (Record x (_,y)) =
      prettyCon p "Record" [prettyPrec App x, prettyPrec App y]

  export
  Pretty UnionType where
    prettyPrec p (UT fst snd rest) =
      prettyParens (p >= App) $
        "Union" <++> prettyList (map pretty $ fst :: snd :: rest)

  export
  Pretty UnionMemberType where
    prettyPrec p (UD (_,x)) = prettyPrec p x
    prettyPrec p (UU x)     = prettyPrec p x

export
Pretty ConstType where
  prettyPrec p (CP x) = prettyPrec p x
  prettyPrec p (CI x) = prettyIdent x

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
data CGType : Type where
  Ident     : Identifier -> CGType
  Idl       : IdlType -> CGType
  UndefOr   : IdlType -> CGType
  Undefined : CGType
  VarArg    : IdlType -> CGType

export
Pretty CGType where
  prettyPrec p (Ident x)   = prettyPrec p x.value
  prettyPrec p (Idl x)     = prettyPrec p x
  prettyPrec p (UndefOr x) = prettySingleCon p "UndefOr" x
  prettyPrec p Undefined   = "Undefined"
  prettyPrec p (VarArg x)  = prettySingleCon p "VarArg" x

export
fromIdl : IdlType -> CGType
fromIdl (D $ NotNull $ P Undefined)        = Undefined
fromIdl (D $ NotNull $ I $ MkIdent "void") = Undefined
fromIdl t                                  = Idl t

export
returnType : CGType -> Doc ()
returnType Undefined = jsio Open "()"
returnType x         = jsio Open x

export
primReturnType : CGType -> Doc ()
primReturnType Undefined = primIO Open "()"
primReturnType x         = primIO Open x
