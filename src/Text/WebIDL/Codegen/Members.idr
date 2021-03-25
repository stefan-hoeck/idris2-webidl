module Text.WebIDL.Codegen.Members

import Data.List
import Text.WebIDL.Codegen.Types
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types
import Text.WebIDL.Encoder as E

export
Pretty FloatLit where
  pretty v = pretty $ E.floatLit v

export
Pretty IntLit where
  pretty (Hex k) = pretty $ E.toDigits "0x" 16 k
  pretty (Oct k) = pretty $ E.toDigits "0o" 8 k
  pretty (I x)   = pretty x

export
Pretty ConstValue where
  pretty (B x) = pretty x
  pretty (F x) = pretty x
  pretty (I x) = pretty x

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

export
constants : List Const -> List (Doc ())
constants = map const . sortBy (comparing name)
  where const : Codegen Const
        const (MkConst t n v) =
          vsep [ ""
               , "public export"
               , pretty n.value <++> ":" <++> pretty t
               , pretty n.value <++> "=" <++> pretty v
               ]

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

primType : (name : String) -> Nat -> IdlType -> Doc ()
primType name n x = typeDecl ("prim__" ++ name) (primReturnType x) $
                      replicate n "AnyPtr"

export
readOnlyAttributes : List (Readonly Attribute) -> List (Doc ())
readOnlyAttributes = map attr . sortBy (comparing name) . map value
  where attr : Codegen Attribute
        attr (MkAttribute _ t n) =
          let primName = "prim__" ++ n.value
           in vsep [ "" 
                   , attrGet n
                   , primType n.value 1 t
                   ]

export
attributes : List Attribute -> List (Doc ())
attributes = map attr . sortBy (comparing name)
  where attr : Codegen Attribute
        attr (MkAttribute _ t n) =
          let primName = "prim__" ++ n.value
           in vsep [ "" 
                   , attrGet n
                   , primType n.value 1 t
                   , ""
                   , attrSet n
                   , primType (setter n.value) 2 t
                   ]
