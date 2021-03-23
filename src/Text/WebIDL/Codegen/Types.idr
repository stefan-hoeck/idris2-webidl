module Text.WebIDL.Codegen.Types

import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types

prettyIdent : Identifier -> Doc ann
prettyIdent (MkIdent "void") = "Undefined"
prettyIdent (MkIdent value) = pretty value

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
  pretty Boolean             = "Bool"
  pretty Byte                = "Int8"
  pretty Octet               = "UInt8"
  pretty BigInt              = "Integer"

export
Pretty StringType where
  pretty ByteString = "String"
  pretty DOMString  = "String"
  pretty USVString  = "String"

export
Pretty a => Pretty (Nullable a) where
  prettyPrec p (MaybeNull x) = prettyCon p "Maybe" [prettyPrec App x]
  prettyPrec p (NotNull x)   = prettyPrec p x

mutual
  export
  Pretty IdlType where
    prettyPrec _ Any         = "JSAny"
    prettyPrec p (D x)       = prettyPrec p x
    prettyPrec p (U x)       = prettyPrec p x
    prettyPrec p (Promise x) = prettyCon p "JSPromise" [prettyPrec App x]

  export
  Pretty Distinguishable where
    prettyPrec p (P x) = prettyPrec p x
    prettyPrec p (S x) = prettyPrec p x
    prettyPrec p (I x) = prettyIdent x
    prettyPrec p (B x) = pretty (show x)
    prettyPrec p (Sequence (_,x)) =
      prettyCon p "JSArray" [prettyPrec App x]
    prettyPrec p (FrozenArray (_,x)) =
      prettyCon p "JSArray" [prettyPrec App x]
    prettyPrec p (ObservableArray (_,x)) =
      prettyCon p "JSArray" [prettyPrec App x]
    prettyPrec p Object = "JSObject"
    prettyPrec p Symbol = "JSSymbol"
    prettyPrec p (Record x (_,y)) =
      prettyCon p "JSRecord" [prettyPrec App x, prettyPrec App y]

  export
  Pretty UnionType where
    prettyPrec p (UT fst snd rest) =
      prettyParens (p >= App) $ "NS I" <++> align (sep $
        ("[" <++> pretty fst) ::
        map (\v => "," <++> pretty v) (snd :: rest) ++ ["]"])

  export
  Pretty UnionMemberType where
    prettyPrec p (UD (_,x)) = prettyPrec p x
    prettyPrec p (UU x)     = prettyPrec p x

export
Pretty ConstType where
  prettyPrec p (CP x) = prettyPrec p x
  prettyPrec p (CI x) = prettyIdent x
