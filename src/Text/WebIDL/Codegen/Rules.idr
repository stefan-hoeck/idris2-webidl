module Text.WebIDL.Codegen.Rules

import Data.List
import Data.List1
import Data.List.Elem
import Data.SortedSet
import Text.WebIDL.Codegen.Util

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

parents : Domain -> List Identifier
parents d =
  mapMaybe inherits d.interfaces ++ mapMaybe inherits d.dictionaries

kinds : List Domain -> SortedMap Identifier Kind
kinds ds =
  let ps = SortedSet.fromList $ ds >>= parents
   in SortedMap.fromList
        $  (ds >>= pairs name KEnum . enums)
        ++ (ds >>= pairs name KMixin . mixins)
        ++ (ds >>= pairs name (iface ps) . interfaces)
        ++ (ds >>= pairs name KDictionary . dictionaries)
        ++ (ds >>= pairs name KCallback . callbackInterfaces)
        ++ (ds >>= pairs name KCallback . callbacks)
        ++ (ds >>= pairs name KAlias . typedefs)
  where
    -- list of identifiers with their kinds
    pairs :
         (a -> Identifier)
      -> (Identifier -> Kind)
      -> List a
      -> List (Identifier,Kind)
    pairs name knd = map $ \v => (name v, knd $ name v)

    iface : SortedSet Identifier -> Identifier -> Kind
    iface ps i = KInterface (contains i ps) i

||| Calculate the environment from a list of domains.
export
env : Nat -> List Domain -> Env
env k ds =
  let ks := kinds ds
   in MkEnv k ks jsTypes (aliases ks $ ds >>= typedefs)
  where
    -- calculates the mapping from type aliases to the
    -- types they represent
    aliases :  SortedMap Identifier Kind
            -> List Typedef
            -> SortedMap Identifier (IdlTypeF ExtAttributeList Kind)
    aliases ks = SortedMap.fromList . map mkPair
      where
        kind : Identifier -> Kind
        kind i = fromMaybe (KOther i) $ lookup i ks

        mkPair : Typedef -> (Identifier,IdlTypeF ExtAttributeList Kind)
        mkPair (MkTypedef _ _ t n) = (n, map kind t)

    dictToType : Dictionary -> (Identifier,JSType)
    dictToType (MkDictionary _ n i _) = (n, MkJSType i Nil)

    interfaceToType : Interface -> (Identifier,JSType)
    interfaceToType (MkInterface _ n i _) = (n, MkJSType i Nil)

    mixin :
         SortedMap Identifier JSType
      -> Includes
      -> SortedMap Identifier JSType
    mixin ts (MkIncludes _ n incl) =
      case lookup n ts of
        Nothing => ts
        Just js => let js2 := {mixins $= (incl ::)} js in insert n js2 ts

    jsTypes : SortedMap Identifier JSType
    jsTypes =
      let types :=
            (ds >>= map dictToType . dictionaries) ++
            (ds >>= map interfaceToType . interfaces)

          includes := ds >>= includeStatements

          initialMap := SortedMap.fromList types

       in foldl mixin initialMap includes

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

buff : BufferRelatedType -> SimpleType
buff Uint8Array        = Unchangeable "UInt8Array"
buff Uint16Array       = Unchangeable "UInt8Array"
buff Uint32Array       = Unchangeable "UInt8Array"
buff Uint8ClampedArray = Unchangeable "UInt8ClampedArray"
buff x                 = Unchangeable $ show x

-- booleans are marshalled from Idris2 `Bool` to JS `Boolean`
-- and back
prim : PrimitiveType -> SimpleType
prim Boolean             = Boolean
prim (Unsigned Short)    = Primitive "Bits16"
prim (Unsigned Long)     = Primitive "Bits32"
prim (Unsigned LongLong) = Primitive "JSBits64"
prim (Signed Short)      = Primitive "Int16"
prim (Signed Long)       = Primitive "Int32"
prim (Signed LongLong)   = Primitive "JSInt64"
prim (Unrestricted x)    = Primitive "Double"
prim (Restricted x)      = Primitive "Double"
prim Undefined           = Undef
prim Byte                = Primitive "Int8"
prim Octet               = Primitive "Bits8"
prim BigInt              = Primitive "Integer"

string : StringType -> SimpleType
string ByteString = Unchangeable "ByteString"
string DOMString  = Primitive "String"
string USVString  = Primitive "String"

strTpe : StringType -> String
strTpe ByteString = "ByteString"
strTpe DOMString = "String"
strTpe USVString = "String"

parameters (e : Env, dom : Domain)

  -- Lookup the kind of an identifier from the environment
  kind : Identifier -> Kind
  kind i = fromMaybe (KOther i) $ lookup i e.kinds

  mutual
    -- the most interesting part when unaliasing a type:
    -- here the aliases may be wrapped in an `I` data
    -- constructor, in which case we try to convert it
    -- to a distinguishable type. If that's not possible,
    -- we return the corresponding type wrapped in a `Left`.
    -- Otherwise we keep the distinguishable type
    -- (but keep unaliasing inner types, if any)
    uaD : DistinguishableF ExtAttributeList Kind -> Codegen CGType
    uaD i@(I $ KAlias x) =
      case lookup x e.aliases of
           Nothing            => Left [UnresolvedAlias dom x]
           Just x             => unalias x
    uaD (I k)                 = Right $ fromKind k
    uaD (Sequence x y)        = simple . Array <$> unalias y
    uaD (FrozenArray x y)     = simple . Array <$> unalias y
    uaD (ObservableArray x y) = simple . Array <$> unalias y
    uaD (Record x _ z)        = simple . Record (strTpe x) <$> unalias z
    uaD (P p)                 = Right . simple $ prim p
    uaD (S s)                 = Right . simple $ string s
    uaD (B b)                 = Right . simple $ buff b
    uaD Object                = Right . simple . Interface True $
                                MkIdent "Object"
    uaD Symbol                = Right $ unchangeable "Symbol"

    uaU :  UnionTypeF ExtAttributeList Kind
        -> Codegen (Nullable $ List1 SimpleType)
    uaU (UT f s r) =
      do (hf ::: tf) <- uaM f
         rest        <- map join (traverse uaM (s ::: r))

         let nullables = hf ::: (tf ++ forget rest)
             simples   = map nullVal nullables

         if any isNullable nullables
            -- the result is nullable
            then pure $ MaybeNull simples
            -- the result is non-nullable
            else pure $ NotNull simples


    -- in case of a wrapped distinguishable type,
    -- we unalias it using `uaD` but keep the unaliased
    -- version only, if it is again distinguishable.
    uaM :  UnionMemberTypeF ExtAttributeList Kind
        -> Codegen (List1 $ Nullable SimpleType)
    uaM (MkUnionMember a t) =
      do t2 <- uaD t
         case t2 of
              Any       => Left [AnyInUnion dom]
              Promise x => Left [PromiseInUnion dom]
              Simple x  => Right $ singleton x
              (Union $ MaybeNull xs) => Right $ map MaybeNull xs
              (Union $ NotNull xs)   => Right $ map NotNull xs

    unalias : IdlTypeF ExtAttributeList Kind -> Codegen CGType
    unalias Any               = Right Any
    unalias (D $ NotNull d)   = uaD d
    unalias (U $ NotNull d)   = Union <$> uaU d
    unalias (U $ MaybeNull d) = Union . nullable <$> uaU d
    unalias (Promise x)       = Promise <$> unalias x
    unalias t@(D $ MaybeNull d) =
      do res <- uaD d
         case res of
              Any         => Left [NullableAny dom]
              (Simple x)  => pure . Simple $ nullable x
              (Union x)   => pure . Union $ nullable x
              (Promise x) => Left [NullablePromise dom]

  -- calculate the aliased type from a type coming
  -- from the WebIDL parser
  -- the unaliased version of the type is only kept (in a `Just`)
  -- if it differs from the original type.
  tpe : IdlType -> Codegen CGType
  tpe t = let cgt = map kind t
           in unalias cgt

  -- convert an IDL type coming from the parser to
  -- a return type in the code generator
  rtpe : IdlType -> Codegen ReturnType
  rtpe (D $ NotNull $ P Undefined)        = Right Undefined
  rtpe (D $ NotNull $ I $ MkIdent "void") = Right Undefined
  rtpe t = Def <$> tpe t

  constTpe : ConstType -> Codegen CGConstType
  constTpe (CI i) =
    case uaD (I $ kind i) of
         Left x                                 => Left x
         Right (Simple $ NotNull $ Primitive s) => Right $ MkConstType s
         Right _                                => Left [InvalidConstType dom]

  constTpe (CP p) =
    case prim p of
         Primitive s => Right $ MkConstType s
         _           => Left [InvalidConstType dom]

  const : Const -> Codegen CGConst
  const (MkConst t n v) = map (\t2 => MkConst t2 n v) (constTpe t)

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

  -- create an optional argument named "value" from
  -- a type coming from the parser
  optArg : IdlType -> Default -> Codegen CGArg
  optArg t d = [| Optional (pure $ MkArgName "value") (tpe t) (pure d) |]

  -- create an argument named "value" from
  -- a type coming from the parser
  valArg : IdlType -> Codegen CGArg
  valArg t = Mandatory (MkArgName "value") <$> tpe t

  -- convert an argument coming from the parser
  -- to one to be used in the code generator
  arg : Arg -> Codegen CGArg
  arg (MkArg _ t n) = Mandatory n <$> tpe t

  -- convert an argument coming from the parser
  -- to a vararg to be used in the code generator
  vararg : Arg -> Codegen CGArg
  vararg (MkArg _ t n) = VarArg n <$> tpe t

  -- convert an argument coming from the parser
  -- to an optional arg to be used in the code generator
  opt : OptArg -> Codegen CGArg
  opt (MkOptArg _ _ t n d) = [| Optional (pure n) (tpe t) (pure d) |]

  -- convert an argument list coming from the parser
  -- to a list of codegen args
  toArgs : ArgumentList -> Codegen Args
  toArgs (VarArg as v)    = [| snoc (traverse arg as) (vararg v) |]
  toArgs (NoVarArg as os) = [| traverse arg as ++ traverse opt os |]

--------------------------------------------------------------------------------
--          Inheritance
--------------------------------------------------------------------------------

  objectOnly : Supertypes
  objectOnly = MkSupertypes [MkIdent "Object"] []

  ||| Calculates the supertypes and mixins for a given
  ||| identifier.
  |||
  |||  @maxIterations : Maximal number of iterations. Without this,
  |||                   the algorithm might loop forever in case of
  |||                   cyclic dependencies. This value corresponds
  |||                   to the maximal length of the inheritance chain.
  supertypes : Identifier -> Supertypes
  supertypes = run e.maxInheritance
    where run : Nat -> Identifier -> Supertypes
          run 0     i = objectOnly
          run (S k) i =
            case lookup i e.jsTypes of
                 Nothing                              => objectOnly

                 (Just $ MkJSType Nothing mixins)       =>
                   { mixins := mixins } objectOnly

                 (Just $ MkJSType (Just parent) mixins) =>
                   let MkSupertypes parents mixins2 = run k parent
                    in MkSupertypes (parent :: parents) (mixins ++ mixins2)

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

  op : Identifier -> Op a -> Codegen (List CGFunction)
  op n (MkOp _ _ Nothing _)   = Left [RegularOpWithoutName dom n]
  op n (MkOp _ t (Just o) as) =
    map pure [| Regular (pure o) (pure $ kind n) (toArgs as) (rtpe t) |]

  static : Identifier -> Op a -> Codegen (List CGFunction)
  static n (MkOp _ _ Nothing _)   = Left [RegularOpWithoutName dom n]
  static n (MkOp _ t (Just o) a) =
    map pure [| Static (pure o) (pure $ kind n) (toArgs a) (rtpe t) |]

  constr : Identifier -> ArgumentList -> Codegen (List CGFunction)
  constr name args = pure . Constructor (kind name) <$> toArgs args

  attrRO : Identifier -> Readonly Attribute -> Codegen (List CGFunction)
  attrRO o (MkRO $ MkAttribute _ t n) =
    pure . AttributeGet n (kind o) <$> rtpe t

  attr : Identifier -> Attribute -> Codegen (List CGFunction)
  attr obj (MkAttribute _ t n) =
     map pure [| Attribute (pure n) (pure $ kind obj) (valArg t) (rtpe t) |]

  str : Identifier -> Stringifier -> Codegen (List CGFunction)
  str o (Z v)              = attr o v
  str o (S $ Z v)          = attrRO o v
  str o (S $ S $ Z v)      = op o v
  str o (S $ S $ S $ Z ()) =
    let name = Just $ MkOpName "toString"
     in op o (MkOp () domString name (NoVarArg [] []))

  staticAttrRO : Identifier -> Readonly Attribute -> Codegen (List CGFunction)
  staticAttrRO o (MkRO $ MkAttribute _ t n) =
    pure . StaticAttributeGet n (kind o) <$> rtpe t

  staticAttr : Identifier -> Attribute -> Codegen (List CGFunction)
  staticAttr obj (MkAttribute _ t n) =
    let ak  = kind obj
     in sequence [ StaticAttributeGet n ak <$> rtpe t
                 , StaticAttributeSet n ak <$> valArg t
                 ]

  dictCon : Kind -> List DictionaryMemberRest -> Codegen CGFunction
  dictCon o = go Nil Nil
    where go : Args -> Args -> List DictionaryMemberRest -> Codegen CGFunction
          go xs ys [] = pure $ DictConstructor o (reverse xs ++ reverse ys)
          go xs ys (Required _ t n :: zs) =
            do t2 <- tpe t
               go (Mandatory (MkArgName n.value) t2 :: xs) ys zs
          go xs ys (Optional t n d :: zs) =
            do t2 <- tpe t
               go xs (Optional (MkArgName n.value) t2 d :: ys) zs

  dictFuns : Dictionary -> Codegen (List CGFunction)
  dictFuns d = [| dictCon (kind d.name) (map snd d.members) ::
                  (map join (traverse (fromMember . snd) d.members)) |]
    where fromMember : DictionaryMemberRest -> Codegen (List CGFunction)
          fromMember (Required _ t n) =
            let an = Right $ MkAttributeName n.value
             in map pure [| Attribute an (pure $ kind d.name)
                                      (valArg t) (rtpe t) |]

          fromMember (Optional t n def) =
            let an = Right $ MkAttributeName n.value
                cgt = map (`UndefOr` Just def) (tpe t)
                ak  = Right $ kind d.name
             in map pure [| Attribute an ak (optArg t def) cgt |]

  mixinFuns : Mixin -> Codegen (List CGFunction)
  mixinFuns m = concat <$> traverse (fromMember . snd) m.members
    where fromMember : MixinMember -> Codegen (List CGFunction)
          fromMember (MConst _)   = Right Nil
          fromMember (MOp o)      = op m.name o
          fromMember (MStr s)     = str m.name s
          fromMember (MAttrRO ro) = attrRO m.name ro
          fromMember (MAttr at)   = attr m.name at

  ifaceFuns : Interface -> Codegen (List CGFunction)
  ifaceFuns i = concat <$> traverse (fromMember . snd) i.members
    where getter : IdlType -> ArgumentList -> Codegen (List CGFunction)
          getter t (NoVarArg [a] Nil) = do
            ag <- arg a
            rt <- rtpe t
            if isIndex (argType ag)
               then Right [Getter (kind i.name) ag rt]
               else Left [InvalidGetter dom i.name]

          getter _ _ = Left [InvalidGetter dom i.name]

          setter : IdlType -> ArgumentList -> Codegen (List CGFunction)
          setter t (NoVarArg [a,r] Nil) = do
            ag <- arg a
            rt <- rtpe t
            rg <- arg r
            if isIndex (argType ag) && isUndefined rt
               then Right [Setter (kind i.name) ag rg]
               else Left [InvalidSetter dom i.name]

          setter _ _ = Left [InvalidSetter dom i.name]

          -- getters and setters without a name are treated as indexed
          -- versions (special syntax in the FFI), all others are treated
          -- as regular operations
          fromOp : Operation -> Codegen (List CGFunction)
          fromOp (MkOp (Just Getter)  t Nothing as) = getter t as
          fromOp (MkOp (Just Setter)  t Nothing as) = setter t as
          fromOp (MkOp (Just Deleter) _ _       _ ) = Right Nil
          fromOp x                                  = op i.name x

          fromPart : PartialInterfaceMember -> Codegen (List CGFunction)
          fromPart (IOp x)                 = fromOp x
          fromPart (IStr x)                = str i.name x
          fromPart (IStatic $ Z x)         = staticAttr i.name x
          fromPart (IStatic $ S $ Z x)     = staticAttrRO i.name x
          fromPart (IStatic $ S $ S $ Z x) = static i.name x
          fromPart (IAttr x)               = attr i.name x
          fromPart (IAttrRO x)             = attrRO i.name x
          fromPart (IConst _)              = Right Nil
          fromPart (IMap x)                = Right Nil
          fromPart (ISet x)                = Right Nil
          fromPart (IMapRO x)              = Right Nil
          fromPart (ISetRO x)              = Right Nil
          fromPart (IAttrInh x)            = Right Nil
          fromPart (IIterable x y)         = Right Nil
          fromPart (IAsync x y xs)         = Right Nil

          fromMember : InterfaceMember -> Codegen (List CGFunction)
          fromMember (Z $ MkConstructor as) = constr i.name as
          fromMember (S $ Z p)              = fromPart p
          fromMember (S $ S x) impossible

  ifaceConsts : Interface -> Codegen (List CGConst)
  ifaceConsts (MkInterface _ _ _ ms) = join <$> traverse (fromMember . snd) ms
    where fromMember : InterfaceMember -> Codegen (List CGConst)
          fromMember (S $ Z $ IConst x) = map pure $ const x
          fromMember _                  = Right []

  mixinConsts : Mixin -> Codegen (List CGConst)
  mixinConsts (MkMixin _ _ ms) = join <$> traverse (fromMember . snd) ms
    where fromMember : MixinMember -> Codegen (List CGConst)
          fromMember (MConst x) = map pure $ const x
          fromMember _          = Right []

  callbackConsts : CallbackInterface -> Codegen (List CGConst)
  callbackConsts (MkCallbackInterface _ _ ms) =
    join <$> traverse (fromMember . snd) ms
    where fromMember : CallbackInterfaceMember -> Codegen (List CGConst)
          fromMember v = case extract Const v of
                              Nothing => Right []
                              Just x  => map pure $ const x

  export
  domain : Codegen CGDomain
  domain = [| MkDomain (pure dom.domain)
                       callbacks
                       (traverse dict dom.dictionaries)
                       (pure dom.enums)
                       (traverse iface dom.interfaces)
                       (traverse mixin dom.mixins)
           |]

    where dict : Dictionary -> Codegen CGDict
          dict v@(MkDictionary _ n i _) =
            MkDict n  (supertypes n) <$> dictFuns v

          iface : Interface -> Codegen CGIface
          iface v@(MkInterface _ n i _) =
            [| MkIface (pure n) (pure $ supertypes n)
                       (ifaceConsts v) (ifaceFuns v) |]

          mixin : Mixin -> Codegen CGMixin
          mixin m = [| MkMixin (pure m.name) (mixinConsts m) (mixinFuns m) |]

          callback : Callback -> Codegen CGCallback
          callback c = [| MkCallback (pure c.name) (pure Nil)
                                     (rtpe c.type) (toArgs c.args) |]

          callbackIface : CallbackInterface -> Codegen CGCallback
          callbackIface v@(MkCallbackInterface _ n ms) =
            case mapMaybe (\(_,m)   => extract RegularOperation m) ms of
                 [MkOp () t _ a] =>
                   [| MkCallback (pure n) (callbackConsts v) (rtpe t) (toArgs a) |]
                 xs => Left [CBInterfaceInvalidOps dom n (length xs)]

          callbacks : Codegen (List CGCallback)
          callbacks = [| traverse callback dom.callbacks ++
                         traverse callbackIface dom.callbackInterfaces |]
