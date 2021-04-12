module Text.WebIDL.Codegen.Members

import Data.List
import Text.WebIDL.Codegen.Args
import Text.WebIDL.Codegen.Rules
import Text.WebIDL.Codegen.Types
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types

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
constants : List CGConst -> List String
constants = map (show . const) . sortBy (comparing name)
  where const : CGConst -> Doc ()
        const (MkConst t n v) =
          indent 2 $ vsep [ ""
                          , "public export"
                          , pretty n.value <++> ":" <++> constTpe Open t
                          , pretty n.value <++> "=" <++> pretty v
                          ]

--------------------------------------------------------------------------------
--          Callback Conversion
--------------------------------------------------------------------------------

export
primCallback : CGCallback -> String
primCallback (MkCallback n _ t as) =
  callbackFFI n (primMarshallCallback n) (callbackFFI $ length as) as t

export
callback : CGCallback -> String
callback (MkCallback n _ t as) =
  callbackAPI n (marshallCallback n) (primMarshallCallback n) as t

--------------------------------------------------------------------------------
--          Attribute
--------------------------------------------------------------------------------

attrRW :  Nat
       -> AttributeName
       -> Kind
       -> CGArg
       -> ReturnType
       -> String
attrRW k n o t rt =
  let implName   = pretty' $ attrGetter k n
      primGet    = pretty' $ primAttrGetter k n
      primSet    = pretty' $ primAttrSetter k n
      msg        = pretty' $ namespacedIdent o (fromString $ "get" ++ n.value)
      po         = pretty' $ kindToString o
      (tpe,impl) = attrImpl msg primGet primSet t
   in show . indent 2 $ vsep [ ""
                             , "export"
                             , hsep [implName,":",po,"->",tpe]
                             , hsep [implName,"=",impl]
                             ]

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

obj : Kind -> CGArg
obj k = Mandatory (MkArgName "obj") (MkAType (identToType k) Nothing)

function : (Nat,CGFunction) -> String
function (k,Getter o i t) = fun o (getter k) (primGetter k) [obj o, i] t

function (k,Setter o i v) =
  fun o (setter k) (primSetter k) [obj o, i, v] Undefined

function (k,Regular n o as t) = fun o (op k n) (primOp k n) (obj o :: as) t

function (k,Static n o as t) = fun o (op k n) (primOp k n) as t

function (k,Attribute n o t rt) = attrRW k n o t rt

function (k,AttributeGet n o t) =
  fun o (attrGetter k n) (primAttrGetter k n) [obj o] t

function (k,StaticAttributeSet n o t) =
  fun o (attrSetter k n) (primAttrSetter k n) [t] Undefined

function (k,StaticAttributeGet n o t) =
  fun o (attrGetter k n) (primAttrGetter k n) [] t

function (k,DictConstructor o as) =
  fun o (constr k) (primConstr k) as (fromKind o)

function (k,Constructor o as) =
  fun o (constr k) (primConstr k) as (fromKind o)

prim : (Nat,CGFunction) -> String
prim (k,Getter o i t) = funFFI (primGetter k) getterFFI [obj o, i] t
prim (k,Setter o i v) = funFFI (primSetter k) setterFFI [obj o, i, v] Undefined
prim (k,Regular n o args t) =
  let as = obj o :: args
   in funFFI (primOp k n) (funFFI n $ length args) as t

prim (k,Static n o as t) =
  funFFI (primOp k n) (staticFunFFI o n $ length as) as t

prim (k,Attribute n o t rt) =
  fastUnlines
    [ funFFI (primAttrGetter k n) (attrGetFFI n) [obj o] rt
    , ""
    , funFFI (primAttrSetter k n) (attrSetFFI n) [obj o, t] Undefined
    ]

prim (k,AttributeGet n o t) =
  funFFI (primAttrGetter k n) (attrGetFFI n) [obj o] t

prim (k,StaticAttributeSet n o t) =
  funFFI (primAttrSetter k n) (staticAttrSetFFI o n) [t] Undefined

prim (k,StaticAttributeGet n o t) =
  funFFI (primAttrGetter k n) (staticAttrGetFFI o n) [] t

prim (k,DictConstructor o as) =
  funFFI (primConstr k) (dictConFFI $ map argName as) as (fromKind o)

prim (k,Constructor o as)  =
  funFFI (primConstr k) (conFFI o $ length as) as (fromKind o)

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
