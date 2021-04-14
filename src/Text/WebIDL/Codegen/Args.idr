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

public export
record PrettyArg where
  constructor MkPrettyArg
  name : ArgumentName
  doc  : Doc ()

export
argIdent : PrettyArg -> IdrisIdent
argIdent = fromString . value . name

public export
PrettyArgs : Type
PrettyArgs = List PrettyArg

--------------------------------------------------------------------------------
--          FFI
--------------------------------------------------------------------------------

nullableFFI :  (Prec -> a -> Doc ()) -> Prec -> Nullable a -> Doc ()
nullableFFI f p (MaybeNull x) = prettyCon p "Nullable" [f App x] 
nullableFFI f p (NotNull x)   = f p x

mutual
  simpleFFI : Prec -> SimpleType -> Doc ()
  simpleFFI p Undef            = "Undefined"
  simpleFFI p Boolean          = "Boolean"
  simpleFFI p (ParentType _ x) = pretty x.value
  simpleFFI p (Primitive x)    = pretty x
  simpleFFI p (Unchangeable x) = pretty x
  simpleFFI p (Enum x)         = "String"
  simpleFFI p (Array x)        = prettyCon p "Array" [ffi App x]
  simpleFFI p (Record x y)     = prettyCon p "Record" [pretty x, ffi App y]

  unionFFI : Prec -> List1 SimpleType -> Doc ()
  unionFFI p ts = prettyCon p ("Union" <+> pretty (length $ forget ts))
                              (map (simpleFFI App) $ forget ts)
  export
  ffi : Prec -> CGType -> Doc ()
  ffi p Any         = "AnyPtr"
  ffi p (Promise x) = prettyCon p "Promise" [ffi App x]
  ffi p (Simple x)  = nullableFFI simpleFFI p x
  ffi p (Union x)   = nullableFFI unionFFI p x

--------------------------------------------------------------------------------
--          API
--------------------------------------------------------------------------------

nullableAPI :  (Prec -> a -> Doc ()) -> Prec -> Nullable a -> Doc ()
nullableAPI f p (MaybeNull x) = prettyCon p "Maybe" [f App x] 
nullableAPI f p (NotNull x)   = f p x

mutual
  simpleAPI : Maybe Nat -> Prec -> SimpleType -> Doc ()
  simpleAPI (Just k) _ (ParentType True x) = pretty "t" <+> pretty k
  simpleAPI _ _        (ParentType _ x) = pretty x.value
  simpleAPI _ _ Undef            = "Undefined"
  simpleAPI _ _ Boolean          = "Bool"
  simpleAPI _ _ (Primitive x)    = pretty x
  simpleAPI _ _ (Unchangeable x) = pretty x
  simpleAPI _ _ (Enum x)         = pretty x.value
  simpleAPI _ p (Array x)        = prettyCon p "Array" [ffi App x]
  simpleAPI _ p (Record x y)     = prettyCon p "Record" [pretty x, ffi App y]

  unionAPI : Prec -> List1 SimpleType -> Doc ()
  unionAPI p (h ::: t) =
    let simple = simpleAPI Nothing Open
        brkt   = align . sep $  ["[" <++> simple h]
                             ++ map ("," <++>) (map simple t)
                             ++ ["]"]
     in prettyCon p "NS I" [brkt]

  api : Maybe Nat -> Prec -> CGType -> Doc ()
  api _ p Any         = "Any"
  api _ p (Promise x) = prettyCon p "Promise" [ffi App x]
  api k p (Simple x)  = nullableAPI (simpleAPI k) p x
  api _ p (Union x)   = nullableAPI unionAPI p x

--------------------------------------------------------------------------------
--          Return Types
--------------------------------------------------------------------------------

export
ret : Prec -> CGType -> Doc ()
ret p (Union $ MaybeNull xs) =
  let u = if all SimpleType.safeCast xs
             then unionAPI App xs
             else unionFFI App xs

   in prettyCon p "Maybe" [u]

ret p t@(Union $ NotNull xs) =
  if all SimpleType.safeCast xs then api Nothing p t else ffi p t

ret p t             = api Nothing p t

returnTypeFFI' : (io : Doc ()) -> ReturnType -> Doc ()
returnTypeFFI' io Undefined     = io <++> "()"
returnTypeFFI' io (Def t)       = prettyCon Open io [ffi App t]
returnTypeFFI' io (UndefOr t _) =
  prettyCon Open io [prettyCon App "UndefOr" [ffi App t]]

returnTypeFFI : ReturnType -> Doc ()
returnTypeFFI = returnTypeFFI' "PrimIO"

returnTypeAPI : ReturnType -> Doc ()
returnTypeAPI Undefined     = "JSIO ()"
returnTypeAPI (Def t)       = prettyCon Open "JSIO" [ret App t]
returnTypeAPI (UndefOr t _) =
  prettyCon Open "JSIO" [prettyCon App "Optional" [ret App t]]

export
constTpe : CGConstType -> Doc ()
constTpe = pretty . primitive

--------------------------------------------------------------------------------
--          Default Arg
--------------------------------------------------------------------------------

export
Pretty StringLit where
  pretty v = pretty v.value

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

defltS : SimpleType -> Default -> Maybe (Doc ())
defltS Boolean (C $ B x)   = Just $ pretty x
defltS (Primitive _) (S x) = Just $ pretty x
defltS (Primitive _) (C x) = Just $ pretty x
defltS _ _                 = Nothing

unionD : Prec -> List1 SimpleType -> Default -> Maybe (Doc ())
unionD p ts d =
  let m = choiceMap (`defltS` d) ts
   in map (\x => prettyCon p "inject" [x]) m

export
deflt : Bool -> Prec -> CGType -> Default -> Maybe (Doc ())
deflt _ p Any Null  = Just $ prettyCon p "MkAny" ["$ null {a = ()}"]
deflt _ p Any (S x) = Just $ prettyCon p "MkAny" [pretty x]
deflt _ p Any (C x) = Just $ prettyCon p "MkAny" [pretty x]
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

argTypeFFI : Prec -> CGArg -> Doc ()
argTypeFFI p (Mandatory _ t)  = ffi p t
argTypeFFI p (VarArg _ t)     = prettyCon p "VarArg" [ffi App t]
argTypeFFI p (Optional _ t _) = prettyCon p "UndefOr" [ffi App t]

argTypeAPI : Nat -> Prec -> CGArg -> Doc ()
argTypeAPI k p (Mandatory _ t)  = api (Just k) p t
argTypeAPI _ p (VarArg _ t)     = prettyCon p "VarArg" [ffi App t]
argTypeAPI k p (Optional _ t _) = prettyCon p "Optional" [api (Just k) App t]

arg : PrettyArg -> Doc ()
arg a = parens $ hsep [pretty (argIdent a), ":", a.doc]

prettyArgFFI : CGArg -> Doc ()
prettyArgFFI = argTypeFFI Open

prettyArgAPI : Nat -> CGArg -> PrettyArg
prettyArgAPI k a = let doc = argTypeAPI k Open a
                    in MkPrettyArg (argName a) doc

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

funTypeFFI : (name : IdrisIdent) -> ReturnType -> Args -> Doc ()
funTypeFFI n t as = typeDecl n (returnTypeFFI t) (map prettyArgFFI as)

funType : (name : IdrisIdent) -> ReturnType -> Args -> Doc ()
funType n t as = 
  let (implicits,autos,explicits) = run 0 as
   in typeDeclWithImplicits n (returnTypeAPI t) implicits (autos ++ explicits)
  where run : Nat -> Args -> (List $ Doc(), List $ Doc (), List $ Doc ())
        run _ []        = ([],[],[])
        run k (a :: as) =
          case CGArg.inheritance a of
               Just (n,_) =>
                 let k2  = S k
                     pk2 = "t" <+> pretty' k2
                     (implicits,autos,explicits) = run k2 as
                     impl = "JSType" <++> pk2
                     aut = hsep ["{auto 0 _ : Elem"
                                , pretty n.value
                                ,"(Types" <++> pk2 <+> ")}"
                                ]
                     expl = arg (prettyArgAPI k2 a)
                  in (impl :: implicits, aut :: autos, expl :: explicits)
               Nothing =>
                 let (implicits,autos,explicits) = run k as
                     expl = arg (prettyArgAPI k a)
                  in (implicits, autos, expl :: explicits)

export
callbackFFI :  (obj  : Identifier)
            -> (name : IdrisIdent)
            -> (impl : String)
            -> (args : Args)
            -> (tpe  : ReturnType)
            -> String
callbackFFI o n impl as t =
  let cbTpe  = functionTypeOnly (returnTypeFFI' "IO" t)
                                (map prettyArgFFI as)

      retTpe = "PrimIO" <++> pretty' (o.value)

   in show . indent 2 $ vsep [ ""
                             , "export"
                             , pretty' impl
                             , typeDecl n retTpe [cbTpe]
                             ]

export
callbackAPI :  (obj  : Identifier)
            -> (name : IdrisIdent)
            -> (prim : IdrisIdent)
            -> (args : Args)
            -> (tpe  : ReturnType)
            -> String
callbackAPI o n prim as t =
  let cbTpe  = functionTypeOnly (returnTypeFFI' "IO" t)
                                (map prettyArgFFI as)

      retTpe = "JSIO" <++> pretty' (o.value)
      impl   = pretty' n <++> "cb = primJS $" <++> pretty prim <++> "cb"

   in show . indent 2 $ vsep [ ""
                             , "export"
                             , typeDecl n retTpe [cbTpe]
                             , impl
                             ]


export
funFFI :  (name : IdrisIdent)
       -> (impl : String)
       -> (args : Args)
       -> (tpe  : ReturnType)
       -> String
funFFI n impl as t =
   show . indent 2 $ vsep ["", "export", pretty impl, funTypeFFI n t as ]

export
namespacedIdent : (ns : Kind) -> (name : IdrisIdent) -> String
namespacedIdent ns n = fastConcat ["\"",kindToString ns,".",show n,"\""]

fun' :  (ns         : Kind)
     -> (name       : IdrisIdent)
     -> (prim       : IdrisIdent)
     -> (args       : Args)
     -> (undefs     : List String)
     -> (returnType : ReturnType)
     -> List (Doc ())
fun' ns name prim as us rt =
  let vs      = take (length as) (unShadowingArgNames name)

      appVs   = align . sep $ zipWith adjVal vs as ++ map pretty' us

      primNS  = kindToString ns ++ "." ++ show prim

      primCall = if sameType rt
                    then "primJS"
                    else "tryJS " ++ namespacedIdent ns name

      lhs     = pretty' . fastConcat . intersperse " " $ show name :: vs
      
      impl    = lhs <++> (align . sep) [ "=" <++> pretty primCall
                                       , "$" <++> pretty primNS <++> appVs
                                       ]
   in ["", "export", funType name rt as, impl]

  where adjVal : String -> CGArg -> Doc ()
        adjVal v a =
          -- If the same type is used at the FFI and the API,
          -- there is no need to convert the value.
          -- If the value should be upcast to a parent type (`inheritance` returns
          -- a `Just`), we upcast the value using a number of nested `map`s,
          -- corresponding to the number of functors around
          -- the type (at most two layers: `Optional` and `Maybe`),
          -- otherwise, we just use `toFFI` to convert it.
          case (sameType a, snd <$> inheritance a) of
            (True,_)            => pretty v
            (False,Nothing)     => parens ("toFFI" <++> pretty v)
            (False,Just Direct) => parens ("up" <++> pretty v)
            (False,Just May)    => parens ("mayUp" <++> pretty v)
            (False,Just Opt)    => parens ("optUp" <++> pretty v)
            (False,Just OptMay) => parens ("omyUp" <++> pretty v)

export
fun :  (ns   : Kind)
    -> (name : IdrisIdent)
    -> (prim : IdrisIdent)
    -> Args
    -> ReturnType
    -> String
fun ns name prim as t =
  let funImpl       = fun' ns name prim as [] t

      -- function without optional args
      as2      = filter (not . isOptional) as
      undefs   = replicate (length as `minus` length as2) "undef"
      funImpl2 = if null undefs then []
                 else fun' ns name2 prim as2 undefs t

   in show . indent 2 $ vsep (funImpl ++ funImpl2)

  where name2 : IdrisIdent
        name2 = case name of
                     II v prf     => fromString $ v ++ "'"
                     Prim v       => Prim (v ++ "'")
                     Underscore v => fromString $ v ++ "'"
