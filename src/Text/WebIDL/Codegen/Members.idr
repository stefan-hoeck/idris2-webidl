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
obj k = Required (MkArgName "obj") (MkAType (identToType k) Nothing)

prettyArgType : CGArg -> Doc ()
prettyArgType (Required _ t)      = pretty t
prettyArgType (OptionalArg _ t _) = prettySingleCon Open "UndefOr" t
prettyArgType (VarArg _ t)        = prettySingleCon Open "VarArg" t

prettyArg : CGArg -> Doc ()
prettyArg a = parens $ hsep [pretty (argIdent a), ":", prettyArgType a]

primType : (name : IdrisIdent) -> Args -> ReturnType -> Doc ()
primType name as t =
  typeDecl name (primReturnType t) (map prettyArgType as)

primFun :  (name : IdrisIdent)
        -> (impl : String)
        -> (args : Args)
        -> (tpe  : ReturnType)
        -> String
primFun name impl as t =
  show . indent 2 $ vsep [ ""
                         , "export"
                         , pretty impl
                         , primType name as t
                         ]

fun :  (ns : Kind)
    -> (name : IdrisIdent)
    -> (prim : IdrisIdent)
    -> Args
    -> ReturnType
    -> String
fun ns name prim args t = show . indent 2 $ vsep ["","export",funType,funImpl] 
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

opImpl : Nat -> OperationName -> Kind -> Args -> ReturnType -> String
opImpl k n o as t =
  let args = obj o :: as
   in show . indent 2 $ vsep [ ""
                             , "export"
                             , pretty $ funFFI n (length args)
                             , primType (primOp k n) args t
                             ]

op : Nat -> OperationName -> Kind -> Args -> ReturnType -> String
op k n o as t =
  let args = obj o :: as
   in fun o (op k n) (primOp k n) args t

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

function : (Nat,CGFunction) -> String
function (k,Getter o i t) = fun o (getter k) (primGetter k) [obj o, i] t

function (k,Setter o i v) =
  fun o (setter k) (primSetter k) [obj o, i, v] Undefined

function (k,Regular n o args t) = op k n o args t

function (k,AttributeSet n o t) =
  fun o (attrSetter k n) (primAttrSetter k n) [obj o, t] Undefined

function (k,AttributeGet n o t) =
  fun o (attrGetter k n) (primAttrGetter k n) [obj o] t

function (k,DictConstructor o as) =
  fun o (constr k) (primConstr k) as (fromKind o)

function (k,Constructor o as) =
  fun o (constr k) (primConstr k) as (fromKind o)

prim : (Nat,CGFunction) -> String
prim (k,Getter o i t) = primFun (primGetter k) getterFFI [obj o, i] t
prim (k,Setter o i v) = primFun (primSetter k) setterFFI [obj o, i, v] Undefined
prim (k,Regular n o args t) = opImpl k n o args t

prim (k,AttributeSet n o t) =
  primFun (primAttrSetter k n) (attrSetFFI n) [obj o, t] Undefined

prim (k,AttributeGet n o t) =
  primFun (primAttrGetter k n) (attrGetFFI n) [obj o] t

prim (k,DictConstructor o as) =
  primFun (primConstr k) (dictConFFI $ map argName as) as (fromKind o)

prim (k,Constructor o as)  =
  primFun (primConstr k) (conFFI o $ length as) as (fromKind o)

-- Tags functions with an index if several function of the
-- same priority (same kind of function and same name) exist,
-- as these would lead to overloading issues.
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
functions = map function . tagFunctions . sortBy (comparing priority)

export
primFunctions : List CGFunction -> List String
primFunctions = map prim . tagFunctions . sortBy (comparing priority)
