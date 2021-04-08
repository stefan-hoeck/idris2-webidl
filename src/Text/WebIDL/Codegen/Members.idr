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

fun :  (ns : Kind)
    -> (name : IdrisIdent)
    -> (prim : IdrisIdent)
    -> Args
    -> ReturnType
    -> Doc ()
fun ns name prim args t = indent 2 $ vsep ["","export",funType,funImpl] 
  where funType : Doc ()
        funType = typeDecl name (returnType t) (map prettyArg args)

        primNS : Doc ()
        primNS = pretty ns <+> "." <+> pretty prim

        funImpl : Doc ()
        funImpl = let vs  = take (length args) (unShadowingArgNames name)
                      lhs = hsep (pretty name :: map pretty vs)
                      rhs = hsep [ "primJS $"
                                 , primNS
                                 , align (sep $ map pretty vs)
                                 ]
                   in lhs <++> "=" <++> rhs

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

attributeSetFFI : Nat -> AttributeName -> Kind -> CGArg -> String
attributeSetFFI k n o t =
   show . indent 2 $ vsep [ ""
                          , "export"
                          , pretty $ attrSetFFI n
                          , primType (primSetter k n) [obj o, t] Undefined
                          ]

attributeGetFFI : Nat -> AttributeName -> Kind -> ReturnType -> String
attributeGetFFI k n o t =
   show . indent 2 $ vsep [ ""
                          , "export"
                          , pretty $ attrGetFFI n
                          , primType (primGetter k n) [obj o] t
                          ]

opFFI : Nat -> OperationName -> Kind -> Args -> ReturnType -> String
opFFI k n o as t =
  let args = obj o :: as
   in show . indent 2 $ vsep [ ""
                             , "export"
                             , pretty $ funFFI n (length args)
                             , primType (primOp k n) args t
                             ]

constructorFFI : Nat -> Kind -> Args -> String
constructorFFI k o args =
  show . indent 2 $ vsep [ ""
                         , "export"
                         , pretty $ conFFI o (length args)
                         , primType (primConstructor k) args (FromIdl $ identToType o)
                         ]

attributeSet : Nat -> AttributeName -> Kind -> CGArg -> String
attributeSet k n o a =
  show $ fun o (setter k n) (primSetter k n) [obj o, a] Undefined

attributeGet : Nat -> AttributeName -> Kind -> ReturnType -> String
attributeGet k n o t =
  show $ fun o (getter k n) (primGetter k n) [obj o] t

op : Nat -> OperationName -> Kind -> Args -> ReturnType -> String
op k n o as t =
  let args = obj o :: as
   in show $ fun o (op k n) (primOp k n) args t

constr : Nat -> Kind -> Args -> String
constr k o as =
  show $ fun o (constr k) (primConstructor k) as (FromIdl $ identToType o)

function : (Nat,CGFunction) -> Maybe String
function (k,AttributeSet n o t) = Just $ attributeSet k n o t
function (k,AttributeGet n o t) = Just $ attributeGet k n o t
function (k,Constructor o args) = Just $ constr k o args
function (k,Regular n o args t) = Just $ op k n o args t

prim : (Nat,CGFunction) -> Maybe String
prim (k,AttributeSet n o a) = Just $ attributeSetFFI k n o a
prim (k,AttributeGet n o t) = Just $ attributeGetFFI k n o t
prim (k,Constructor n args) = Just $ constructorFFI k n args
prim (k,Regular n o args t) = Just $ opFFI k n o args t

tagFunctions : List CGFunction -> List (Nat,CGFunction)
tagFunctions = go 0
  where go : Nat -> List CGFunction -> List (Nat,CGFunction)
        go _ []        = []
        go k (x :: []) = [(k,x)]
        go k (x :: t@(y :: ys)) =
          if priority x == priority y
             then (k,x) :: go (S k) t
             else (k,x) :: go 0 t

export
functions : List CGFunction -> List String
functions = mapMaybe function . tagFunctions . sortBy (comparing priority)

export
primFunctions : List CGFunction -> List String
primFunctions = mapMaybe prim . tagFunctions . sortBy (comparing priority)
