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

mutual
  unaliasDist : Env -> CGDist -> (Either CGType CGDist)
  unaliasDist e i@(I $ KAlias x) =
    case lookup x e.aliases of
         Nothing              => Right i
         Just (D $ NotNull v) => Right v
         Just x               => Left x

  unaliasDist e (Sequence x y) = Right (Sequence x $ unalias e y)
  unaliasDist e (FrozenArray x y) = Right (FrozenArray x $ unalias e y)
  unaliasDist e (ObservableArray x y) = Right (ObservableArray x $ unalias e y)
  unaliasDist e (Record x y z) = Right (Record x y $ unalias e z)
  unaliasDist _ d = Right d

  unaliasUnion : Env -> CGUnion -> CGUnion
  unaliasUnion e (UT f s r) =
    UT (unaliasMember e f) (unaliasMember e s) (map (unaliasMember e) r)

  unaliasMember : Env -> CGMember -> CGMember
  unaliasMember e (UD y n) = case unaliasDist e (nullVal n) of
                                  (Left _)  => UD y n -- no other way to break out
                                  (Right x) => UD y (n $> x)
  unaliasMember e (UU y)   = UU $ map (unaliasUnion e) y

  unalias : Env -> CGType -> CGType
  unalias e Any = Any
  unalias e t@(D $ MaybeNull d) = either (const t) (D . MaybeNull) $ unaliasDist e d
  unalias e (D $ NotNull d) = either id (D . NotNull) $ unaliasDist e d
  unalias e (U x) = U $ map (unaliasUnion e) x
  unalias e (Promise x) = Promise $ unalias e x

-- calculate the aliased type from a type coming
-- from the WebIDL parser
tpe : Env -> IdlType -> AType
tpe e t = let cgt = map (kind e) t
              al  = unalias e cgt
           in MkAType cgt (if al == cgt then Nothing else Just al)

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

-- create an optional argument named "value" from
-- a type coming from the parser
optArg : Env -> IdlType -> Default -> CGArg
optArg e t = Optional (MkArgName "value") (tpe e t)

-- create an argument named "value" from
-- a type coming from the parser
valArg : Env -> IdlType -> CGArg
valArg e t = Mandatory (MkArgName "value") (tpe e t)

-- convert an argument coming from the parser
-- to one to be used in the code generator
arg : Env -> Arg -> CGArg
arg e (MkArg _ t n) = Mandatory n (tpe e t)

-- convert an argument coming from the parser
-- to a vararg to be used in the code generator
vararg : Env -> Arg -> CGArg
vararg e (MkArg _ t n) = VarArg n (tpe e t)

-- convert an argument coming from the parser
-- to an optional arg to be used in the code generator
opt : Env -> OptArg -> CGArg
opt e (MkOptArg _ _ t n d) = Optional n (tpe e t) d

-- convert an argument list coming from the parser
-- to a list of codegen args
toArgs : Env -> ArgumentList -> Args
toArgs e (VarArg as v)    = map (arg e) as ++ [vararg e v]
toArgs e (NoVarArg as os) = map (arg e) as ++ map (opt e) os

-- convert an IDL type coming from the parser to
-- a return type in the code generator
rtpe : Env -> IdlType -> ReturnType
rtpe _ (D $ NotNull $ P Undefined) = Undefined
rtpe _ (D $ NotNull $ I $ MkIdent "void") = Undefined
rtpe e t = FromIdl (tpe e t)

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
supertypes : Env -> Identifier -> Supertypes
supertypes e = run e.maxInheritance
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

fromOp : Env -> Domain -> Identifier -> Op a -> CodegenV (List CGFunction)
fromOp e dom ident (MkOp _ _ Nothing _) =
  Invalid [RegularOpWithoutName dom ident]

fromOp e dom ident (MkOp _ t (Just op) args) = 
  Valid [Regular op (kind e ident) (toArgs e args) (rtpe e t)]

fromStaticOp : Env -> Domain -> Identifier -> Op a -> CodegenV (List CGFunction)
fromStaticOp e dom ident (MkOp _ _ Nothing _) =
  Invalid [RegularOpWithoutName dom ident]

fromStaticOp e dom ident (MkOp _ t (Just op) args) = 
  Valid [Static op (kind e ident) (toArgs e args) (rtpe e t)]

fromConstructor : Env -> Identifier -> ArgumentList -> CodegenV (List CGFunction)
fromConstructor e name args =
  Valid [Constructor (kind e name) $ toArgs e args]

fromAttrRO : Env -> Identifier -> Readonly Attribute -> CodegenV (List CGFunction)
fromAttrRO e obj (MkRO $ MkAttribute _ t n) =
  Valid [AttributeGet n (kind e obj) $ (rtpe e t)]

fromAttr : Env -> Identifier -> Attribute -> CodegenV (List CGFunction)
fromAttr e obj (MkAttribute _ t n) =
  let cgt = rtpe e t
      ak  = kind e obj
   in Valid [AttributeGet n ak cgt, AttributeSet n ak (valArg e t)]

fromStr : Env -> Domain -> Identifier -> Stringifier -> CodegenV (List CGFunction)
fromStr e _ obj (Z v)              = fromAttr e obj v
fromStr e _ obj (S $ Z v)          = fromAttrRO e obj v
fromStr e d obj (S $ S $ Z v)      = fromOp e d obj v
fromStr e d obj (S $ S $ S $ Z ()) =
  let name = Just $ MkOpName "toString"
   in fromOp e d obj (MkOp () domString name (NoVarArg [] []))

fromStaticAttrRO : Env -> Identifier -> Readonly Attribute -> CodegenV (List CGFunction)
fromStaticAttrRO e obj (MkRO $ MkAttribute _ t n) =
  Valid [StaticAttributeGet n (kind e obj) $ (rtpe e t)]

fromStaticAttr : Env -> Identifier -> Attribute -> CodegenV (List CGFunction)
fromStaticAttr e obj (MkAttribute _ t n) =
  let cgt = rtpe e t
      ak  = kind e obj
   in Valid [StaticAttributeGet n ak cgt, StaticAttributeSet n ak (valArg e t)]

dictCon : Env -> Kind -> List DictionaryMemberRest -> CGFunction
dictCon e o = go Nil Nil
  where go : Args -> Args -> List DictionaryMemberRest -> CGFunction
        go xs ys [] = DictConstructor o (reverse xs ++ reverse ys)
        go xs ys (Required _ t n :: zs) =
          go (Mandatory (MkArgName n.value) (tpe e t) :: xs) ys zs
        go xs ys (Optional t n d :: zs) =
          go xs (Optional (MkArgName n.value) (tpe e t) d :: ys) zs

dictFuns : Env -> Dictionary -> List CGFunction
dictFuns e d = dictCon e (kind e d.name) (map snd d.members) ::
               (d.members >>= fromMember . snd)
  where fromMember : DictionaryMemberRest -> List CGFunction
        fromMember (Required _ t n) =
          let an = MkAttributeName n.value
              cgt = rtpe e t
              ak  = kind e d.name
           in [ AttributeGet an ak cgt , AttributeSet an ak (valArg e t) ]

        fromMember (Optional t n def) =
          let an = MkAttributeName n.value
              cgt = UndefOr (tpe e t) (Just def)
              ak  = kind e d.name
           in [ AttributeGet an ak cgt
              , AttributeSet an ak (optArg e t def)
              ]

mixinFuns : Env -> Domain -> Mixin -> CodegenV (List CGFunction)
mixinFuns e dom m = concat <$> traverse (fromMember . snd) m.members
  where fromMember : MixinMember -> CodegenV (List CGFunction)
        fromMember (MConst _)   = Valid Nil
        fromMember (MOp op)     = fromOp e dom m.name op
        fromMember (MStr s)     = fromStr e dom m.name s
        fromMember (MAttrRO ro) = fromAttrRO e m.name ro
        fromMember (MAttr at)   = fromAttr e m.name at

ifaceFuns : Env -> Domain -> Interface -> CodegenV (List CGFunction)
ifaceFuns e dom i = concat <$> traverse (fromMember . snd) i.members
  where getter : IdlType -> ArgumentList -> CodegenV (List CGFunction)
        getter t (NoVarArg [a] Nil) =
          let ag = arg e a
           in if isIndex (argType ag)
                   then Valid [Getter (kind e i.name) ag (rtpe e t)]
                   else Invalid [InvalidGetter dom i.name]
        getter _ _ = Invalid [InvalidGetter dom i.name]

        setter : IdlType -> ArgumentList -> CodegenV (List CGFunction)
        setter t (NoVarArg [a,r] Nil) =
          let ag = arg e a
           in if isIndex (argType ag) && isUndefined (rtpe e t)
                 then Valid [Setter (kind e i.name) ag (arg e r)]
                 else Invalid [InvalidSetter dom i.name]
        setter _ _ = Invalid [InvalidSetter dom i.name]

        fromMember : InterfaceMember -> CodegenV (List CGFunction)
        fromMember (Z $ MkConstructor args) = fromConstructor e i.name args
        fromMember (S $ Z $ IConst x)       = Valid Nil

        -- getters and setters without a name are treated as indexed
        -- versions (special syntax in the FFI), all others are treated
        -- as regular operations
        fromMember (S $ Z $ IOp x)          =
          case x of
               MkOp (Just Getter)  t Nothing as => getter t as
               MkOp (Just Setter)  t Nothing as => setter t as
               MkOp (Just Deleter) _ _       _  => Valid Nil
               MkOp _              t n       as => fromOp e dom i.name x

        fromMember (S $ Z $ IStr x)                = fromStr e dom i.name x
        fromMember (S $ Z $ IStatic $ Z x)         = fromStaticAttr e i.name x
        fromMember (S $ Z $ IStatic $ S $ Z x)     = fromStaticAttrRO e i.name x
        fromMember (S $ Z $ IStatic $ S $ S $ Z x) = fromStaticOp e dom i.name x
        fromMember (S $ Z $ IAttr x)               = fromAttr e i.name x
        fromMember (S $ Z $ IMap x)                = Valid Nil
        fromMember (S $ Z $ ISet x)                = Valid Nil
        fromMember (S $ Z $ IAttrRO x)             = fromAttrRO e i.name x
        fromMember (S $ Z $ IMapRO x)              = Valid Nil
        fromMember (S $ Z $ ISetRO x)              = Valid Nil
        fromMember (S $ Z $ IAttrInh x)            = Valid Nil
        fromMember (S $ Z $ IIterable x y)         = Valid Nil
        fromMember (S $ Z $ IAsync x y xs)         = Valid Nil
        fromMember (S $ S x) impossible

ifaceConstants : Interface -> List Const
ifaceConstants (MkInterface _ _ _ ms) = mapMaybe (fromMember . snd) ms
  where fromMember : InterfaceMember -> Maybe Const
        fromMember (S $ Z $ IConst x) = Just x
        fromMember _                  = Nothing

mixinConstants : Mixin -> List Const
mixinConstants (MkMixin _ _ ms) = mapMaybe (fromMember . snd) ms
  where fromMember : MixinMember -> Maybe Const
        fromMember (MConst x) = Just x
        fromMember _          = Nothing

callbackConstants : CallbackInterface -> List Const
callbackConstants (MkCallbackInterface _ _ ms) =
  mapMaybe (\(_,v) => extract Const v) ms

export
domain : Env -> Domain -> CodegenV CGDomain
domain e d = [| MkDomain (pure d.domain)
                         (callbacks d.callbacks d.callbackInterfaces)
                         (traverse dict d.dictionaries)
                         (pure d.enums)
                         (traverse iface d.interfaces)
                         (traverse mixin d.mixins)
             |]

  where dict : Dictionary -> CodegenV CGDict
        dict v@(MkDictionary _ n i _) =
          Valid $ MkDict n  (supertypes e n) (dictFuns e v)

        iface : Interface -> CodegenV CGIface
        iface v@(MkInterface _ n i _) =
          MkIface n (supertypes e n) (ifaceConstants v) <$>
            ifaceFuns e d v

        mixin : Mixin -> CodegenV CGMixin
        mixin v@(MkMixin _ n _) =
          MkMixin n (mixinConstants v) <$> mixinFuns e d v

        callback : Callback -> CodegenV CGCallback
        callback (MkCallback _ n t args) = Valid $ MkCallback n Nil t args

        callbackIface : CallbackInterface -> CodegenV CGCallback
        callbackIface v@(MkCallbackInterface _ n ms) =
          case mapMaybe (\(_,m)   => extract RegularOperation m) ms of
               [MkOp () t _ a] => Valid $ MkCallback n (callbackConstants v) t a
                 
               xs => Invalid [CBInterfaceInvalidOps d n (length xs)]

        callbacks :  List Callback
                  -> List CallbackInterface
                  -> CodegenV (List CGCallback)
        callbacks cs cis =
          [| traverse callback cs ++ traverse callbackIface cis |]
