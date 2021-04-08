module Text.WebIDL.Codegen.Members

import Data.List
import Text.WebIDL.Codegen.Rules
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
  pretty (Oct 0) = pretty "0"
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
jsType : Identifier -> Supertypes -> String
jsType n (MkSupertypes parents ms) =
  let mixins = sortedNubOn id ms

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
--          Functions
--------------------------------------------------------------------------------

obj : Kind -> CGArg
obj = Regular (MkArgName "obj") . identToType

prettyArgType : CGArg -> Doc ()
prettyArgType (Regular _ t)       = pretty t
prettyArgType (OptionalArg _ t _) = prettySingleCon Open "UndefOr" t
prettyArgType (VarArg _ t)        = prettySingleCon Open "VarArg" t

prettyArg : CGArg -> Doc ()
prettyArg a = parens $ hsep [pretty (argIdent a), ":", prettyArgType a]

primType : (name : IdrisIdent) -> Args -> ReturnType -> Doc ()
primType name as t =
  typeDecl name (primReturnType t) (map prettyArgType as)

funType : (name : IdrisIdent) -> Args -> ReturnType -> Doc ()
funType name as t = typeDecl name (returnType t) (map prettyArg as)

fun :  (name : IdrisIdent)
    -> (prim : IdrisIdent)
    -> Args
    -> ReturnType
    -> Doc ()
fun name prim args t = indent 2 $ vsep [ ""
                                       , "export"
                                       , funType name args t
                                       ] 

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

attributeSetFFI : AttributeName -> Kind -> CGArg -> String
attributeSetFFI n o t =
   show $ vsep [ ""
               , "export"
               , pretty $ attrSetFFI n
               , primType (primSetter n o) [obj o, t] Undefined
               ]

attributeGetFFI : AttributeName -> Kind -> ReturnType -> String
attributeGetFFI n o t =
   show $ vsep [ ""
               , "export"
               , pretty $ attrGetFFI n
               , primType (primGetter n o) [obj o] t
               ]

opFFI : OperationName -> Kind -> Args -> ReturnType -> String
opFFI n o as t =
  let args = obj o :: as
   in show $ vsep [ ""
                  , pretty $ funFFI n (length args)
                  , primType (primOp n o) args t
                  ]

constructorFFI : Kind -> Args -> String
constructorFFI o args =
  show $ vsep [ ""
              , pretty $ conFFI o (length args)
              , primType (primConstructor o) args (FromIdl $ identToType o)
              ]

attributeSet : AttributeName -> Kind -> CGArg -> String
attributeSet n o a =
  show $ fun (setter n) (primSetter n o) [obj o, a] Undefined

attributeGet : AttributeName -> Kind -> ReturnType -> String
attributeGet n o t = show $ fun (getter n) (primGetter n o) [obj o] t

op : OperationName -> Kind -> Args -> ReturnType -> String
op n o as t =
  let args = obj o :: as
   in show $ fun (fromString $ n.value) (primOp n o) args t

constr : Kind -> Args -> String
constr o as = show $ fun "new" (primConstructor o) as (FromIdl $ identToType o)

function : CGFunction -> Maybe String
function (AttributeSet n o t) = Just $ attributeSet n o t
function (AttributeGet n o t) = Just $ attributeGet n o t
function (Constructor o args) = Nothing
function (Regular n o args t) = Just $ op n o args t

prim : CGFunction -> Maybe String
prim (AttributeSet n o a) = Just $ attributeSetFFI n o a
prim (AttributeGet n o t) = Just $ attributeGetFFI n o t
prim (Constructor n args) = Just $ constructorFFI n args
prim (Regular n o args t) = Just $ opFFI n o args t

export
functions : List CGFunction -> List String
functions = mapMaybe function . sortBy (comparing priority)

export
primFunctions : List CGFunction -> List String
primFunctions = mapMaybe prim . sortedNubOn priority
