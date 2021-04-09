module Text.WebIDL.Codegen.Args

import Text.WebIDL.Codegen.Types
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types

--------------------------------------------------------------------------------
--          Pretty Types
--------------------------------------------------------------------------------

export
Pretty Kind where
  pretty = pretty . value . ident

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
  Pretty b => Pretty (IdlTypeF a b) where
    prettyPrec _ Any         = "AnyPtr"
    prettyPrec p (D x)       = prettyPrec p x
    prettyPrec p (U x)       = prettyPrec p x
    prettyPrec p (Promise x) = prettyCon p "Promise" [prettyPrec App x]

  export
  Pretty b => Pretty (DistinguishableF a b) where
    prettyPrec p (P x) = prettyPrec p x
    prettyPrec p (S x) = prettyPrec p x
    prettyPrec p (I x) = prettyPrec p x
    prettyPrec p (B x) = prettyPrec p x
    prettyPrec p (Sequence _ x) =
      prettyCon p "Array" [prettyPrec App x]
    prettyPrec p (FrozenArray _ x) =
      prettyCon p "Array" [prettyPrec App x]
    prettyPrec p (ObservableArray _ x) =
      prettyCon p "Array" [prettyPrec App x]
    prettyPrec p Object = "Object"
    prettyPrec p Symbol = "Symbol"
    prettyPrec p (Record x _ y) =
      prettyCon p "Record" [prettyPrec App x, prettyPrec App y]

  export
  Pretty b => Pretty (UnionTypeF a b) where
    prettyPrec p (UT fst snd rest) =
      let args = map (prettyPrec App) (fst :: snd :: rest)
          con  = pretty $ "Union" ++ show (length args)
       in prettyCon p con args

  export
  Pretty b => Pretty (UnionMemberTypeF a b) where
    prettyPrec p (UD _ x) = prettyPrec p x
    prettyPrec p (UU x)     = prettyPrec p x

export
Pretty Identifier where pretty = pretty . value

export
Pretty a => Pretty (ConstTypeF a) where
  prettyPrec p (CP x) = prettyPrec p x
  prettyPrec p (CI x) = prettyPrec p x

export
Pretty AType where
  prettyPrec p = prettyPrec p . type

export
Pretty ReturnType where
  prettyPrec p Undefined     = "()"
  prettyPrec p (UndefOr x _) = prettySingleCon p "UndefOr" x
  prettyPrec p (FromIdl x)   = prettyPrec p x

export
returnType : ReturnType -> Doc ()
returnType = jsio Open

export
primReturnType : ReturnType -> Doc ()
primReturnType = primIO Open