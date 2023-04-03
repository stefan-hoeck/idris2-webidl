module Text.WebIDL.Codegen.Members

import Data.List
import Text.WebIDL.Codegen.Args
import Text.WebIDL.Codegen.Rules
import Text.WebIDL.Codegen.Types
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types

%default total

--------------------------------------------------------------------------------
--          Subtyping
--------------------------------------------------------------------------------

export
jsType : Identifier -> Supertypes -> String
jsType n (MkSupertypes parents ms) =
  let mixins = sortedNubOn id ms

   in render80 $ vsep
        [ empty
        , line "public export"
        , line "JSType \{n} where"
        , indent 2 $ prettyCon Open "parents =" [list (map (line . value) parents)]
        , empty
        , indent 2 $ prettyCon Open "mixins =" [list (map (line . value) mixins)]
        ]

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

export
constants : List CGConst -> List String
constants = map (render80 . const) . sortBy (comparing name)
  where
    const : {opts : _} -> CGConst -> Doc opts
    const (MkConst t n v) =
      indent 2 $ vsep
        [ empty
        , line "public export"
        , line "\{n} :" <++> constTpe t
        , line "\{n} =" <++> prettyConst v
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

attrImpl:
     {opts : _}
  -> (msg : Doc opts)
  -> (set : Doc opts)
  -> (get : Doc opts)
  -> (app : Doc opts)
  -> (arg : CGArg)
  -> (Doc opts, Doc opts)
attrImpl msg s g a (Mandatory _ (Simple $ MaybeNull x)) =
  ( line "Attribute False Maybe" <++> ret App (Simple $ NotNull x)
  , prettyCon Open "fromNullablePrim" [msg,s,g,a]
  )

attrImpl msg s g a (Mandatory _ (Union $ MaybeNull x)) =
  ( line "Attribute False Maybe" <++> ret App (Union $ NotNull x)
  , prettyCon Open "fromNullablePrim" [msg,s,g,a]
  )

attrImpl msg s g a (Mandatory _ t) =
  ( line "Attribute True Prelude.id" <++> ret App t
  , prettyCon Open "fromPrim" [msg,s,g,a]
  )

attrImpl msg s g a (VarArg _ t) =
  ( line "Attribute True Prelude.id" <++> prettyCon App "VarArg" [ffi App t]
  , prettyCon Open "fromPrim" [msg,s,g,a]
  )

attrImpl msg s g a (Optional _ t d) =
   case deflt (safeCast t) App t d of
      Nothing  =>
        ( line "Attribute False Optional" <++> ret App t
        , prettyCon Open "fromUndefOrPrimNoDefault" [msg,s,g,a]
        )
      Just x =>
        ( line "Attribute True Optional" <++> ret App t
        , prettyCon Open "fromUndefOrPrim" [msg,s,g,x,a]
        )

attrRW :
     Nat
  -> AttributeName
  -> Kind
  -> CGArg
  -> ReturnType
  -> String
attrRW k n o t rt =
  let implName := attrGetter k n
      primGet  := line "\{primAttrGetter k n}"
      primSet  := line "\{primAttrSetter k n}"
      msg      := namespacedIdent o (fromString $ "get" ++ n.value)
      po       := kindToString o
      up       := if isParent o then "(v :> \{po})" else "v"

      (tpe,impl) := attrImpl (line msg) primGet primSet (line up) t
      funTpe     := if isParent o
                      then typeDecl
                             implName
                             tpe
                             [ line "{auto 0 _ : JSType t}"
                             , line "{auto 0 _ : Elem \{po} (Types t)}"
                             , line "t"
                             ]
                      else typeDecl implName tpe [line "\{po}"]

   in render80 . indent 2 $ vsep
        [ empty
        , line "export"
        , funTpe
        , line "\{implName} v =" <++> impl
        ]

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

obj : Kind -> CGArg
obj k = Mandatory (MkArgName "obj") (fromKind k)

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

hasVarArg : Args -> Bool
hasVarArg (VarArg _ _ :: []) = True
hasVarArg []                 = False
hasVarArg (_ :: xs)          = hasVarArg xs

prim : (Nat,CGFunction) -> String
prim (k,Getter o i t) = funFFI (primGetter k) getterFFI [obj o, i] t
prim (k,Setter o i v) = funFFI (primSetter k) setterFFI [obj o, i, v] Undefined
prim (k,Regular n o args t) =
  let as = obj o :: args
   in if hasVarArg args
         then funFFI (primOp k n) (funFFIVarArg n $ length args) as t
         else funFFI (primOp k n) (funFFI n $ length args) as t

prim (k,Static n o as t) =
  if hasVarArg as
     then funFFI (primOp k n) (staticFunFFIVarArg o n $ length as) as t
     else funFFI (primOp k n) (staticFunFFI o n $ length as) as t

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
  if hasVarArg as
     then funFFI (primConstr k) (conFFIVarArg o $ length as) as (fromKind o)
     else funFFI (primConstr k) (conFFI o $ length as) as (fromKind o)

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
