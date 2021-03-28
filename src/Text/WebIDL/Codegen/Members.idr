module Text.WebIDL.Codegen.Members

import Data.List
import Text.WebIDL.Codegen.Inheritance
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
--          Subtyping
--------------------------------------------------------------------------------

export
jsType : JSTypes -> (maxIterations : Nat) -> Identifier -> String
jsType ts mi n =
  let MkSupertypes parents ms = supertypes ts mi n

      mixins = sortedNubOn id ms

      doc = indent {ann = ()} 2 $
              vsep [ ""
                   , "public export"
                   , "JSType" <++> pretty n.value <++> "where"
                   , "  parents = " <++>
                       prettyList (map (pretty . value) parents)
                   , ""
                   , "  mixins = " <++>
                       prettyList (map (pretty . value) mixins)
                   ]
   in show doc

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

export
constants : List Const -> List String
constants = map (show . const) . sortBy (comparing name)
  where const : Const -> Doc ()
        const (MkConst t n v) =
          indent 2 $ vsep [ ""
                          , "public export"
                          , pretty n.value <++> ":" <++> pretty t
                          , pretty n.value <++> "=" <++> pretty v
                          ]

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

primType : (name : IdrisIdent) -> Nat -> IdlType -> Doc ()
primType name n x = typeDecl (Prim $ show name) (primReturnType x) $
                      replicate n "AnyPtr"

-- We need to make sure that implicits are added only once, though.
funType : (name : IdrisIdent) -> ArgumentList -> IdlType -> Doc ()
funType n args t =
  let args2 = map (toPrettyParam . snd) args

   in typeDeclWithImplicits n (returnType t) [] args2

  where toPrettyParam : ArgumentRest -> Doc ()
        toPrettyParam (Optional (e,tpe) (MkArgName n) def) =
          prettyArg (fromString n) (pretty tpe)

        toPrettyParam (Mandatory tpe (MkArgName n)) =
          prettyArg (fromString n) (pretty tpe)

        -- TODO: Properly support varargs
        toPrettyParam (VarArg tpe (MkArgName n)) =
          prettyArg (fromString n) (pretty tpe)


readonly : Identifier -> Attribute -> List (Doc ())
readonly i (MkAttribute _ t (MkAttributeName n)) =
  let ii = fromString n
   in [ ""
      , pretty $ attrGetFFI n
      , primType ii 1 t
      , ""
      , "export"
      , funType ii [objArg i] t
      ]

readwrite : Identifier -> Attribute -> List (Doc ())
readwrite i a@(MkAttribute _ t (MkAttributeName n)) =
  readonly i a ++ [ ""
                  , pretty $ attrSetFFI n
                  , primType (setter n) 2 t
                  , ""
                  , "export"
                  , funType (setter n) [objArg i, valArg t] undefined
                  ]

-- TODO: Change Identifier to IdrisIdent here
export
readOnlyAttributes :  Identifier
                   -> List (Readonly Attribute)
                   -> List String
readOnlyAttributes i = map (show . indent 2 . vsep . readonly i) 
                     . sortBy (comparing name) 
                     . map value

-- TODO: Change Identifier to IdrisIdent here
export
attributes : Identifier -> List Attribute -> List String
attributes i = map (show . indent 2 . vsep . readwrite i) 
             . sortBy (comparing name)
