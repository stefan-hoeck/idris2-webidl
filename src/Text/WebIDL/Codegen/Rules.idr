module Text.WebIDL.Codegen.Rules

import Data.List
import Data.List.Elem
import Text.WebIDL.Codegen.Util

--------------------------------------------------------------------------------
--          Environment
--------------------------------------------------------------------------------

||| Calculate the environment from a list of domains.
export
env : Nat -> List Domain -> Env
env k ds = let kinds = SortedMap.fromList
                     $  (ds >>= pairs name KEnum . enums)
                     ++ (ds >>= pairs name KMixin . mixins)
                     ++ (ds >>= pairs name KInterface . interfaces)
                     ++ (ds >>= pairs name KDictionary . dictionaries)
                     ++ (ds >>= pairs name KCallback . callbackInterfaces)
                     ++ (ds >>= pairs name KCallback . callbacks)
                     ++ (ds >>= pairs name KAlias . typedefs)

            in MkEnv k kinds jsTypes (aliases kinds $ ds >>= typedefs)
  where -- pairs list of identifiers with their kinds
        pairs :  (a -> Identifier)
              -> (Identifier -> Kind)
              -> List a
              -> List (Identifier,Kind)
        pairs name knd = map \v => (name v, knd $ name v)

        -- calculates the mapping from type aliases to the
        -- types they represent
        aliases :  SortedMap Identifier Kind
                -> List Typedef
                -> SortedMap Identifier CGType
        aliases ks = SortedMap.fromList . map mkPair
          where kind : Identifier -> Kind
                kind i = fromMaybe (KOther i) $ lookup i ks

                mkPair : Typedef -> (Identifier,CGType)
                mkPair (MkTypedef _ _ t n) = (n, map kind t)

        dictToType : Dictionary -> (Identifier,JSType)
        dictToType (MkDictionary _ n i _) = (n, MkJSType i Nil)

        interfaceToType : Interface -> (Identifier,JSType)
        interfaceToType (MkInterface _ n i _) = (n, MkJSType i Nil)

        mixin :  SortedMap Identifier JSType
              -> Includes
              -> SortedMap Identifier JSType
        mixin ts (MkIncludes _ n incl) =
          case lookup n ts of
               Nothing => ts
               Just js => let js2 = record {mixins $= (incl ::)} js
                           in insert n js2 ts

        jsTypes : SortedMap Identifier JSType
        jsTypes = let types =  (ds >>= map dictToType . dictionaries)
                            ++ (ds >>= map interfaceToType . interfaces)

                      includes = ds >>= includeStatements

                      initialMap = SortedMap.fromList types

                   in foldl mixin initialMap includes

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

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
    uaD : CGDist -> (Either CGType CGDist)
    uaD i@(I $ KAlias x) =
      case lookup x e.aliases of
           Nothing              => Right i
           Just (D $ NotNull v) => Right v
           Just x               => Left x
  
    uaD (Sequence x y)        = Right (Sequence x $ unalias y)
    uaD (FrozenArray x y)     = Right (FrozenArray x $ unalias y)
    uaD (ObservableArray x y) = Right (ObservableArray x $ unalias y)
    uaD (Record x y z)        = Right (Record x y $ unalias z)
    uaD d                     = Right d
  
    -- boring: We just unalias the members
    uaU : CGUnion -> CGUnion
    uaU (UT f s r) = UT (uaM f) (uaM s) (map uaM r)
  
    -- in case of a wrapped distinguishable type,
    -- we unalias it using `uaD` but keep the unaliased
    -- version only, if it is again distinguishable.
    uaM : CGMember -> CGMember
    uaM (UU y)   = UU $ map uaU y
    uaM (UD y n) = case uaD (nullVal n) of
                        (Left _)  => UD y n -- no other way to break out
                        (Right x) => UD y (n $> x)
  
    -- nullable types are only unliased, if the unaliase
    -- type is distinguishable. non-nullable types are always
    -- fully unaliased.
    unalias : CGType -> CGType
    unalias Any = Any
    unalias t@(D $ MaybeNull d) = either (const t) (D . MaybeNull) $ uaD d
    unalias (D $ NotNull d) = either id (D . NotNull) $ uaD d
    unalias (U x) = U $ map uaU x
    unalias (Promise x) = Promise $ unalias x
  
  -- calculate the aliased type from a type coming
  -- from the WebIDL parser
  -- the unaliased version of the type is only kept (in a `Just`)
  -- if it differs from the original type.
  tpe : IdlType -> AType
  tpe t = let cgt = map kind t
              al  = unalias cgt
           in MkAType cgt (if al == cgt then Nothing else Just al)
  
  -- convert an IDL type coming from the parser to
  -- a return type in the code generator
  rtpe : IdlType -> ReturnType
  rtpe (D $ NotNull $ P Undefined) = Undefined
  rtpe (D $ NotNull $ I $ MkIdent "void") = Undefined
  rtpe t = FromIdl (tpe t)

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

  -- create an optional argument named "value" from
  -- a type coming from the parser
  optArg : IdlType -> Default -> CGArg
  optArg t = Optional (MkArgName "value") (tpe t)
  
  -- create an argument named "value" from
  -- a type coming from the parser
  valArg : IdlType -> CGArg
  valArg t = Mandatory (MkArgName "value") (tpe t)
  
  -- convert an argument coming from the parser
  -- to one to be used in the code generator
  arg : Arg -> CGArg
  arg (MkArg _ t n) = Mandatory n (tpe t)
  
  -- convert an argument coming from the parser
  -- to a vararg to be used in the code generator
  vararg : Arg -> CGArg
  vararg (MkArg _ t n) = VarArg n (tpe t)
  
  -- convert an argument coming from the parser
  -- to an optional arg to be used in the code generator
  opt : OptArg -> CGArg
  opt (MkOptArg _ _ t n d) = Optional n (tpe t) d
  
  -- convert an argument list coming from the parser
  -- to a list of codegen args
  toArgs : ArgumentList -> Args
  toArgs (VarArg as v)    = map arg as ++ [vararg v]
  toArgs (NoVarArg as os) = map arg as ++ map opt os

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

                 Just $ MkJSType Nothing mixins       =>
                   record { mixins = mixins } objectOnly

                 Just $ MkJSType (Just parent) mixins =>
                   let MkSupertypes parents mixins2 = run k parent
                    in MkSupertypes (parent :: parents) (mixins ++ mixins2)

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

  op : Identifier -> Op a -> CodegenV (List CGFunction)
  op n (MkOp _ _ Nothing _)   = Invalid [RegularOpWithoutName dom n]
  op n (MkOp _ t (Just o) as) = Valid [Regular o (kind n) (toArgs as) (rtpe t)]

  static : Identifier -> Op a -> CodegenV (List CGFunction)
  static n (MkOp _ _ Nothing _)   = Invalid [RegularOpWithoutName dom n]
  static n (MkOp _ t (Just o) a) = Valid [Static o (kind n) (toArgs a) (rtpe t)]

  constr : Identifier -> ArgumentList -> CodegenV (List CGFunction)
  constr name args = Valid [Constructor (kind name) $ toArgs args]

  attrRO : Identifier -> Readonly Attribute -> CodegenV (List CGFunction)
  attrRO o (MkRO $ MkAttribute _ t n) = Valid [AttributeGet n (kind o) (rtpe t)]

  attr : Identifier -> Attribute -> CodegenV (List CGFunction)
  attr obj (MkAttribute _ t n) =
    let ak  = kind obj
     in Valid [AttributeGet n ak (rtpe t), AttributeSet n ak (valArg t)]

  str : Identifier -> Stringifier -> CodegenV (List CGFunction)
  str o (Z v)              = attr o v
  str o (S $ Z v)          = attrRO o v
  str o (S $ S $ Z v)      = op o v
  str o (S $ S $ S $ Z ()) =
    let name = Just $ MkOpName "toString"
     in op o (MkOp () domString name (NoVarArg [] []))

  staticAttrRO : Identifier -> Readonly Attribute -> CodegenV (List CGFunction)
  staticAttrRO o (MkRO $ MkAttribute _ t n) =
    Valid [StaticAttributeGet n (kind o) $ (rtpe t)]

  staticAttr : Identifier -> Attribute -> CodegenV (List CGFunction)
  staticAttr obj (MkAttribute _ t n) =
    let cgt = rtpe t
        ak  = kind obj
     in Valid [StaticAttributeGet n ak cgt, StaticAttributeSet n ak (valArg t)]

  dictCon : Kind -> List DictionaryMemberRest -> CGFunction
  dictCon o = go Nil Nil
    where go : Args -> Args -> List DictionaryMemberRest -> CGFunction
          go xs ys [] = DictConstructor o (reverse xs ++ reverse ys)
          go xs ys (Required _ t n :: zs) =
            go (Mandatory (MkArgName n.value) (tpe t) :: xs) ys zs
          go xs ys (Optional t n d :: zs) =
            go xs (Optional (MkArgName n.value) (tpe t) d :: ys) zs

  dictFuns : Dictionary -> List CGFunction
  dictFuns d = dictCon (kind d.name) (map snd d.members) ::
                 (d.members >>= fromMember . snd)
    where fromMember : DictionaryMemberRest -> List CGFunction
          fromMember (Required _ t n) =
            let an = MkAttributeName n.value
                ak  = kind d.name
             in [ AttributeGet an ak (rtpe t) , AttributeSet an ak (valArg t) ]

          fromMember (Optional t n def) =
            let an = MkAttributeName n.value
                cgt = UndefOr (tpe t) (Just def)
                ak  = kind d.name
             in [ AttributeGet an ak cgt , AttributeSet an ak (optArg t def) ]

  mixinFuns : Mixin -> CodegenV (List CGFunction)
  mixinFuns m = concat <$> traverse (fromMember . snd) m.members
    where fromMember : MixinMember -> CodegenV (List CGFunction)
          fromMember (MConst _)   = Valid Nil
          fromMember (MOp o)      = op m.name o
          fromMember (MStr s)     = str m.name s
          fromMember (MAttrRO ro) = attrRO m.name ro
          fromMember (MAttr at)   = attr m.name at

  ifaceFuns : Interface -> CodegenV (List CGFunction)
  ifaceFuns i = concat <$> traverse (fromMember . snd) i.members
    where getter : IdlType -> ArgumentList -> CodegenV (List CGFunction)
          getter t (NoVarArg [a] Nil) =
            let ag = arg a
             in if isIndex (argType ag)
                     then Valid [Getter (kind i.name) ag (rtpe t)]
                     else Invalid [InvalidGetter dom i.name]

          getter _ _ = Invalid [InvalidGetter dom i.name]

          setter : IdlType -> ArgumentList -> CodegenV (List CGFunction)
          setter t (NoVarArg [a,r] Nil) =
            let ag = arg a
             in if isIndex (argType ag) && isUndefined (rtpe t)
                   then Valid [Setter (kind i.name) ag (arg r)]
                   else Invalid [InvalidSetter dom i.name]

          setter _ _ = Invalid [InvalidSetter dom i.name]

          -- getters and setters without a name are treated as indexed
          -- versions (special syntax in the FFI), all others are treated
          -- as regular operations
          fromOp : Operation -> CodegenV (List CGFunction)
          fromOp (MkOp (Just Getter)  t Nothing as) = getter t as
          fromOp (MkOp (Just Setter)  t Nothing as) = setter t as
          fromOp (MkOp (Just Deleter) _ _       _ ) = Valid Nil
          fromOp x                                  = op i.name x

          fromPart : PartialInterfaceMember -> CodegenV (List CGFunction)
          fromPart (IOp x)                 = fromOp x
          fromPart (IStr x)                = str i.name x
          fromPart (IStatic $ Z x)         = staticAttr i.name x
          fromPart (IStatic $ S $ Z x)     = staticAttrRO i.name x
          fromPart (IStatic $ S $ S $ Z x) = static i.name x
          fromPart (IAttr x)               = attr i.name x
          fromPart (IAttrRO x)             = attrRO i.name x
          fromPart (IConst _)              = Valid Nil
          fromPart (IMap x)                = Valid Nil
          fromPart (ISet x)                = Valid Nil
          fromPart (IMapRO x)              = Valid Nil
          fromPart (ISetRO x)              = Valid Nil
          fromPart (IAttrInh x)            = Valid Nil
          fromPart (IIterable x y)         = Valid Nil
          fromPart (IAsync x y xs)         = Valid Nil

          fromMember : InterfaceMember -> CodegenV (List CGFunction)
          fromMember (Z $ MkConstructor as) = constr i.name as
          fromMember (S $ Z p)              = fromPart p
          fromMember (S $ S x) impossible

  ifaceConsts : Interface -> List Const
  ifaceConsts (MkInterface _ _ _ ms) = mapMaybe (fromMember . snd) ms
    where fromMember : InterfaceMember -> Maybe Const
          fromMember (S $ Z $ IConst x) = Just x
          fromMember _                  = Nothing

  mixinConsts : Mixin -> List Const
  mixinConsts (MkMixin _ _ ms) = mapMaybe (fromMember . snd) ms
    where fromMember : MixinMember -> Maybe Const
          fromMember (MConst x) = Just x
          fromMember _          = Nothing

  callbackConsts : CallbackInterface -> List Const
  callbackConsts (MkCallbackInterface _ _ ms) =
    mapMaybe (\(_,v) => extract Const v) ms

  export
  domain : CodegenV CGDomain
  domain = [| MkDomain (pure dom.domain)
                       callbacks
                       (traverse dict dom.dictionaries)
                       (pure dom.enums)
                       (traverse iface dom.interfaces)
                       (traverse mixin dom.mixins)
           |]

    where dict : Dictionary -> CodegenV CGDict
          dict v@(MkDictionary _ n i _) =
            Valid $ MkDict n  (supertypes n) (dictFuns v)

          iface : Interface -> CodegenV CGIface
          iface v@(MkInterface _ n i _) =
            MkIface n (supertypes n) (ifaceConsts v) <$> ifaceFuns v

          mixin : Mixin -> CodegenV CGMixin
          mixin m = MkMixin m.name (mixinConsts m) <$> mixinFuns m

          callback : Callback -> CodegenV CGCallback
          callback c = Valid $ MkCallback c.name Nil c.type c.args

          callbackIface : CallbackInterface -> CodegenV CGCallback
          callbackIface v@(MkCallbackInterface _ n ms) =
            case mapMaybe (\(_,m)   => extract RegularOperation m) ms of
                 [MkOp () t _ a] => Valid $ MkCallback n (callbackConsts v) t a
                 xs => Invalid [CBInterfaceInvalidOps dom n (length xs)]

          callbacks : CodegenV (List CGCallback)
          callbacks = [| traverse callback dom.callbacks ++
                         traverse callbackIface dom.callbackInterfaces |]
