||| We need at least three different code generators for
||| WebIDL types and arguments. When used in the FFI, they should
||| be mapped to external types and primitives. In API functions,
||| however, they should be mapped to types convenient for
||| users of the API. Same for return types, but there, the rules
||| about what is possible are different.
|||
||| Finally, types affect how we adjust values and return values
||| in function implementations.
module Text.WebIDL.Codegen.Args

import Data.List
import Data.List1
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Encoder as E

%default total

public export
record PrettyArg (opts : LayoutOpts) where
  constructor MkPrettyArg
  name  : ArgumentName
  doc   : Doc opts

export
argIdent : PrettyArg opts -> IdrisIdent
argIdent = fromString . value . name

public export
0 PrettyArgs : (opts : LayoutOpts) -> Type
PrettyArgs = List . PrettyArg

--------------------------------------------------------------------------------
--          FFI
--------------------------------------------------------------------------------

parameters {opts : LayoutOpts}

  nullableSimpleFFI :  Prec -> Nullable SimpleType -> Doc opts

  nullableUnionFFI :  Prec -> Nullable (List1 SimpleType) -> Doc opts

  simpleFFI : Prec -> SimpleType -> Doc opts

  simpleFFIs : Prec -> List SimpleType -> List (Doc opts)

  unionFFI : Prec -> List1 SimpleType -> Doc opts

  export
  ffi : Prec -> CGType -> Doc opts


  simpleFFI p Undef            = line "Undefined"
  simpleFFI p Boolean          = line "Boolean"
  simpleFFI p (Interface _ x)  = line x.value
  simpleFFI p (Dictionary x)   = line x.value
  simpleFFI p (Mixin x)        = line x.value
  simpleFFI p (Primitive x)    = line x
  simpleFFI p (Unchangeable x) = line x
  simpleFFI p (Enum x)         = line "String"
  simpleFFI p (Array x)        = prettyCon p "Array" [ffi App x]
  simpleFFI p (Record x y)     = prettyCon p "Record" [line x, ffi App y]

  simpleFFIs p []     = []
  simpleFFIs p (h::t) = simpleFFI p h :: simpleFFIs p t

  unionFFI p (h:::t) =
    prettyCon p "Union\{show . S $ length t}"
      (simpleFFI App h :: simpleFFIs App t)

  ffi p Any         = "AnyPtr"
  ffi p (Promise x) = prettyCon p "Promise" [ffi App x]
  ffi p (Simple x)  = nullableSimpleFFI p x
  ffi p (Union x)   = nullableUnionFFI p x

  nullableSimpleFFI p (MaybeNull x) = prettyCon p "Nullable" [simpleFFI App x]
  nullableSimpleFFI p (NotNull x) = simpleFFI p x

  nullableUnionFFI p (MaybeNull x) = prettyCon p "Nullable" [unionFFI App x]
  nullableUnionFFI p (NotNull x)   = unionFFI p x

--------------------------------------------------------------------------------
--          API
--------------------------------------------------------------------------------


  simpleAPI : Maybe Nat -> Prec -> SimpleType -> Doc opts
  simpleAPI (Just k) _ (Dictionary _)     = line "t\{show k}"
  simpleAPI (Just k) _ (Mixin _)          = line "t\{show k}"
  simpleAPI (Just k) _ (Interface True _) = line "t\{show k}"
  simpleAPI Nothing _ (Dictionary x)      = line x.value
  simpleAPI Nothing _ (Mixin x)           = line x.value
  simpleAPI _ _        (Interface _ x)    = line x.value
  simpleAPI _ _ Undef                     = line "Undefined"
  simpleAPI _ _ Boolean                   = line "Bool"
  simpleAPI _ _ (Primitive x)             = line x
  simpleAPI _ _ (Unchangeable x)          = line x
  simpleAPI _ _ (Enum x)                  = line x.value
  simpleAPI _ p (Array x)                 = prettyCon p "Array" [ffi App x]
  simpleAPI _ p (Record x y)              = prettyCon p "Record" [line x, ffi App y]

  unionAPI : Prec -> List1 SimpleType -> Doc opts
  unionAPI p (h ::: t) =
    prettyCon p "NS I" [list $ map (simpleAPI Nothing Open) (h::t)]

  nullableAPI :  (Prec -> a -> Doc opts) -> Prec -> Nullable a -> Doc opts
  nullableAPI f p (MaybeNull x) = prettyCon p "Maybe" [f App x]
  nullableAPI f p (NotNull x)   = f p x

  api : Maybe Nat -> Prec -> CGType -> Doc opts
  api _ p Any         = "Any"
  api _ p (Promise x) = prettyCon p "Promise" [ffi App x]
  api k p (Simple x)  = nullableAPI (simpleAPI k) p x
  api _ p (Union x)   = nullableAPI unionAPI p x

--------------------------------------------------------------------------------
--          Return Types
--------------------------------------------------------------------------------

  export
  ret : Prec -> CGType -> Doc opts
  ret p (Union $ MaybeNull xs) =
    let u :=
      if all SimpleType.safeCast xs
         then unionAPI App xs
         else unionFFI App xs

     in prettyCon p "Maybe" [u]

  ret p t@(Union $ NotNull xs) =
    if all SimpleType.safeCast xs then api Nothing p t else ffi p t

  ret p t             = api Nothing p t

  returnTypeFFI' : (io : String) -> ReturnType -> Doc opts
  returnTypeFFI' io Undefined     = line "\{io} ()"
  returnTypeFFI' io (Def t)       = prettyCon Open io [ffi App t]
  returnTypeFFI' io (UndefOr t _) =
    prettyCon Open io [prettyCon App "UndefOr" [ffi App t]]

  returnTypeFFI : ReturnType -> Doc opts
  returnTypeFFI = returnTypeFFI' "PrimIO"

  returnTypeAPI : ReturnType -> Doc opts
  returnTypeAPI Undefined     = line "JSIO ()"
  returnTypeAPI (Def t)       = prettyCon Open "JSIO" [ret App t]
  returnTypeAPI (UndefOr t _) =
    prettyCon Open "JSIO" [prettyCon App "Optional" [ret App t]]

  export
  constTpe : CGConstType -> Doc opts
  constTpe = line . primitive

--------------------------------------------------------------------------------
--          Default Arg
--------------------------------------------------------------------------------

  export
  prettyConst : ConstValue -> Doc opts
  prettyConst (B x) = pretty x
  prettyConst (F x) = line "\{x}"
  prettyConst (I x) = line "\{x}"

  defltS : SimpleType -> Default -> Maybe (Doc opts)
  defltS Boolean (C $ B x)   = Just $ pretty x
  defltS (Primitive _) (S x) = Just . line $ interpolate x
  defltS (Primitive _) (C x) = Just $ prettyConst x
  defltS _ _                 = Nothing

  unionD : Prec -> List1 SimpleType -> Default -> Maybe (Doc opts)
  unionD p ts d =
    let m = choiceMap (`defltS` d) ts
     in map (\x => prettyCon p "inject" [x]) m

  export
  deflt : Bool -> Prec -> CGType -> Default -> Maybe (Doc opts)
  deflt _ p Any Null  = Just $ prettyCon p "MkAny" [line "$ null {a = ()}"]
  deflt _ p Any (S x) = Just $ prettyCon p "MkAny" [line "\{x}"]
  deflt _ p Any (C x) = Just $ prettyCon p "MkAny" [prettyConst x]
  deflt _ _ (Simple $ MaybeNull x) Null = Just "Nothing"

  deflt _ p (Simple $ MaybeNull x) d =
    map (\v => prettyCon p "Just" [v]) (defltS x d)

  deflt _ _ (Simple $ NotNull x) d = defltS x d

  deflt _ p (Union $ MaybeNull x) Null = Just "Nothing"

  deflt True p (Union $ MaybeNull x) d =
    map (\v => prettyCon p "Just" [v]) (unionD App x d)

  deflt True p (Union $ NotNull x) d = unionD p x d
  deflt _ _ _ _ = Nothing

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

  argTypeFFI : Prec -> CGArg -> Doc opts
  argTypeFFI p (Mandatory _ t)  = ffi p t
  argTypeFFI p (VarArg _ t)     =
    prettyCon p "IO" [prettyCon App "Array" [ffi App t]]
  argTypeFFI p (Optional _ t _) = prettyCon p "UndefOr" [ffi App t]

  argTypeAPI : Nat -> Prec -> CGArg -> Doc opts
  argTypeAPI k p (Mandatory _ t)  = api (Just k) p t
  argTypeAPI _ p (VarArg _ t)     = prettyCon p "List" [api Nothing App t]
  argTypeAPI k p (Optional _ t _) = prettyCon p "Optional" [api (Just k) App t]

  arg : PrettyArg opts -> Doc opts
  arg a = parens $ hsep [line "\{argIdent a}", ":", a.doc]

  prettyArgFFI : CGArg -> Doc opts
  prettyArgFFI = argTypeFFI Open

  prettyArgAPI : Nat -> CGArg -> PrettyArg opts
  prettyArgAPI k a =
    let doc := argTypeAPI k Open a
     in MkPrettyArg (argName a) doc

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

  funTypeFFI : (name : IdrisIdent) -> ReturnType -> Args -> Doc opts
  funTypeFFI n t as = typeDecl n (returnTypeFFI t) (map prettyArgFFI as)

  funType : (name : IdrisIdent) -> ReturnType -> Args -> Doc opts
  funType n t as =
     typeDecl n (returnTypeAPI t) (run 0 as [<] [<] [<])
    where
      run : Nat -> Args -> (imp,aut,expl : SnocList $ Doc opts) -> List (Doc opts)
      run _ []      is aus es = is <>> aus <>> es <>> []
      run k (a::as) is aus es = case CGArg.inheritance a of
        Just (n,_) =>
          let k2  := S k
              pk2 := "t\{show k2}"
              impl := line "{auto 0 _ : JSType \{pk2}}"
              aut  := line "{auto 0 _ : Elem \{n} (Types \{pk2})}"
              expl = arg (prettyArgAPI k2 a)
           in run k2 as (is :< impl) (aus :< aut) (es :< expl)
        Nothing =>
          let expl := arg (prettyArgAPI k a)
           in run (S k) as is aus (es :< expl)

export
callbackFFI :
     (obj  : Identifier)
  -> (name : IdrisIdent)
  -> (impl : String)
  -> (args : Args)
  -> (tpe  : ReturnType)
  -> String
callbackFFI o n impl as t =
  let cbTpe  := functionTypeOnly (returnTypeFFI' "IO" t) (map prettyArgFFI as)
      retTpe := line "PrimIO \{o}"
   in render80 . indent 2 $ vsep
        [ line ""
        , line "export"
        , line "\{impl}"
        , typeDecl n retTpe [cbTpe]
        ]

export
callbackAPI :
     (obj  : Identifier)
  -> (name : IdrisIdent)
  -> (prim : IdrisIdent)
  -> (args : Args)
  -> (tpe  : ReturnType)
  -> String
callbackAPI o n prim as t =
  let cbTpe  = functionTypeOnly (returnTypeFFI' "IO" t)
                                (map prettyArgFFI as)

      retTpe = line "JSIO \{o}"
      impl   = line "\{n} cb = primJS $ \{prim} cb"

   in render80 . indent 2 $ vsep
        [ line ""
        , line "export"
        , typeDecl n retTpe [cbTpe]
        , impl
        ]

export
funFFI :
     (name : IdrisIdent)
  -> (impl : String)
  -> (args : Args)
  -> (tpe  : ReturnType)
  -> String
funFFI n impl as t = render80 . indent 2 $
  vsep [line "", line "export", line impl, funTypeFFI n t as ]

export
namespacedIdent : (ns : Kind) -> (name : IdrisIdent) -> String
namespacedIdent ns n = #""\#{kindToString ns}.\#{n}""#

fun' :
     {opts       : _}
  -> (ns         : Kind)
  -> (name       : IdrisIdent)
  -> (prim       : IdrisIdent)
  -> (args       : Args)
  -> (undefs     : List String)
  -> (returnType : ReturnType)
  -> List (Doc opts)
fun' ns name prim as us rt =
  let vs      := take (length as) (unShadowingArgNames name)

      appVs   := zipWith adjVal vs as ++ map line us

      primNS  := "\{kindToString ns}.\{prim}"

      primCall := if sameType rt
                    then "primJS"
                    else "tryJS " ++ namespacedIdent ns name

      lhs     := unwords $ "\{name}" :: vs

      firstLine := line "\{lhs} = \{primCall} $"

      rhs     := prettyCon Open primNS appVs

      sl      := firstLine <++> rhs

      impl    := ifMultiline sl (vappend firstLine $ indent 2 rhs)

   in [line "", line "export", funType name rt as, impl]

  where
    adjVal : String -> CGArg -> Doc opts
    adjVal v a =
      -- If the same type is used at the FFI and the API,
      -- there is no need to convert the value.
      -- If the value should be upcast to a parent type (`inheritance` returns
      -- a `Just`), we upcast the value using a number of nested `map`s,
      -- corresponding to the number of functors around
      -- the type (at most two layers: `Optional` and `Maybe`),
      -- otherwise, we just use `toFFI` to convert it.
      case (sameType a, snd <$> inheritance a) of
        (True,_)            => line v
        (False,Nothing)     => parens ("toFFI" <++> line v)
        (False,Just Direct) => parens ("up" <++> line v)
        (False,Just May)    => parens ("mayUp" <++> line v)
        (False,Just Opt)    => parens ("optUp" <++> line v)
        (False,Just OptMay) => parens ("omyUp" <++> line v)

export
fun :
     (ns   : Kind)
  -> (name : IdrisIdent)
  -> (prim : IdrisIdent)
  -> Args
  -> ReturnType
  -> String
fun ns name prim as t =
  let funImpl       = fun' ns name prim as [] t

      -- function without optional args
      as2      = filter (not . isOptional) as
      undefs   = List.replicate (length as `minus` length as2) "undef"
      funImpl2 = if null undefs then []
                 else fun' ns name2 prim as2 undefs t

   in render80 . indent 2 $ vsep (funImpl ++ funImpl2)

  where
    name2 : IdrisIdent
    name2 = case name of
      II v         => fromString $ v ++ "'"
      Prim v       => Prim (v ++ "'")
      Underscore v => fromString $ v ++ "'"
