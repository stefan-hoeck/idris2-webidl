module Text.WebIDL.Codegen.Rules

import Data.List
import Data.List.Elem
import Data.Validated
import Data.SortedMap
import Data.SortedSet
import Text.WebIDL.Types
import Text.WebIDL.Codegen.Types
import Text.WebIDL.Codegen.Util
import Text.PrettyPrint.Prettyprinter

||| An external, un-parameterized Javascript type, represented
||| by an identifier. Such a type comes with a parent
||| type (given as an `inheritance` value in the spec)
||| and a number of mixed in types.
|||
||| The actual name of the type is not included, as the set
||| of types is given in `Env` as as `SortedMap`.
public export
record JSType where
  constructor MkJSType
  parent : Maybe Identifier
  mixins : List Identifier

||| The set of external un-parameterized types from the
||| whole spec.
public export
JSTypes : Type
JSTypes = SortedMap Identifier JSType

jsTypes : List Domain -> JSTypes
jsTypes ds =
  let types =  (ds >>= map dictToType . dictionaries)
            ++ (ds >>= map interfaceToType . interfaces)

      includes = ds >>= includeStatements

      initialMap = SortedMap.fromList types

   in foldl mixin initialMap includes

  where dictToType : Dictionary -> (Identifier,JSType)
        dictToType (MkDictionary _ n i _) = (n, MkJSType i Nil)

        interfaceToType : Interface -> (Identifier,JSType)
        interfaceToType (MkInterface _ n i _) = (n, MkJSType i Nil)

        mixin : JSTypes -> Includes -> JSTypes
        mixin ts (MkIncludes _ n incl) =
          case lookup n ts of
               Nothing => ts
               Just js => let js2 = record {mixins $= (incl ::)} js
                           in insert n js2 ts

public export
record Env where
  constructor MkEnv
  maxInheritance : Nat
  kinds          : SortedMap Identifier Kind
  jsTypes        : JSTypes

export
env : Nat -> List Domain -> Env
env k ds = let ts = jsTypes ds
               kinds = SortedMap.fromList
                     $  (ds >>= map (\v => (v.name, KEnum v.name)) . enums)
            in MkEnv k kinds ts


kind : Env -> Identifier -> Kind
kind e i = fromMaybe (KOther i) $ lookup i e.kinds

tpe : Env -> IdlType -> CGType
tpe e = map (kind e)

optArg : Env -> IdlType -> Default -> CGArg
optArg e t = OptionalArg (MkArgName "value") (tpe e t)

valArg : Env -> IdlType -> CGArg
valArg e t = Required (MkArgName "value") (tpe e t)

arg : Env -> Arg -> CGArg
arg e (MkArg _ t n) = Required n (tpe e t)

vararg : Env -> Arg -> CGArg
vararg e (MkArg _ t n) = VarArg n (tpe e t)

opt : Env -> OptArg -> CGArg
opt e (MkOptArg _ _ t n d) = OptionalArg n (tpe e t) d

toArgs : Env -> ArgumentList -> Args
toArgs e (VarArg as v)    = map (arg e) as ++ [vararg e v]
toArgs e (NoVarArg as os) = map (arg e) as ++ map (opt e) os

rtpe : Env -> IdlType -> ReturnType
rtpe _ (D $ NotNull $ P Undefined) = Undefined
rtpe _ (D $ NotNull $ I $ MkIdent "void") = Undefined
rtpe e t = FromIdl (tpe e t)

||| The parent types and mixins of a type. This is
||| used by the code generator to implement the
||| `JS.Inheritance.JSType` instances.
public export
record Supertypes where
  constructor MkSupertypes
  parents : List Identifier
  mixins  : List Identifier

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

--------------------------------------------------------------------------------
--          Codegen Errors
--------------------------------------------------------------------------------

public export
data CodegenErr : Type where
  CBInterfaceInvalidOps  : Domain -> Identifier -> Nat -> CodegenErr
  InvalidGetter          : Domain -> Identifier -> CodegenErr
  InvalidSetter          : Domain -> Identifier -> CodegenErr
  RegularOpWithoutName   : Domain -> Identifier -> CodegenErr

public export
Codegen : Type -> Type
Codegen = Either (List CodegenErr)

public export
CodegenV : Type -> Type
CodegenV = Validated (List CodegenErr)

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

||| A function, for which we will generate some code.
public export
data CGFunction : Type where
  ||| An attribute setter.
  AttributeSet :  (name : AttributeName)
               -> (obj  : Kind)
               -> (tpe  : CGArg)
               -> CGFunction

  ||| An attribute getter.
  AttributeGet :  (name : AttributeName)
               -> (obj  : Kind)
               -> (tpe  : ReturnType)
               -> CGFunction

  ||| An indexed getter.
  Getter       :  (name  : Maybe OperationName)
               -> (obj   : Kind)
               -> (index : CGArg)
               -> (tpe   : ReturnType)
               -> CGFunction

  ||| An indexed setter.
  Setter       :  (name  : Maybe OperationName)
               -> (obj   : Kind)
               -> (index : CGArg)
               -> (value : CGArg)
               -> CGFunction

  ||| An interface constructor with (possibly) optional arguments.
  Constructor  :  (obj : Kind) -> (args : Args) -> CGFunction

  ||| An interface constructor with (possibly) optional arguments.
  DictConstructor : (obj : Kind) -> (args : Args) -> CGFunction

  ||| A regular function with (possibly) optional arguments.
  Regular      :  OperationName
               -> (obj : Kind)
               -> Args
               -> ReturnType
               -> CGFunction

||| This is used for sorting lists of functions to
||| the determine the order in which they appear
||| in the generated code.
|||
||| Attributes will come first, sorted by name,
||| setters, getters, and unsetter grouped together in
||| that order.
|||
||| All other functions come later and will be sorted by name.
export
priority : CGFunction -> (Nat,String,Nat)
priority (DictConstructor n _) = (0,value (ident n),0)
priority (Constructor n _)     = (0,value (ident n),0)
priority (Getter n _ _ _)      = (1,show n,0)
priority (Setter n _ _ _)      = (1,show n,1)
priority (AttributeSet n _ _)  = (2,show n,1)
priority (AttributeGet n _ _)  = (2,show n,0)
priority (Regular n o _ _)     = (3,n.value ++ value (ident o),0)

fromRegular :  Env
            -> Domain
            -> Identifier
            -> RegularOperation
            -> CodegenV (List CGFunction)
fromRegular e dom ident (MkOp () _ Nothing _) =
  Invalid [RegularOpWithoutName dom ident]

fromRegular e dom ident (MkOp () t (Just op) args) = 
  Valid [Regular op (kind e ident) (toArgs e args) (rtpe e t)]

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

dictCon : Env -> Kind -> List DictionaryMemberRest -> CGFunction
dictCon e o = go Nil Nil
  where go : Args -> Args -> List DictionaryMemberRest -> CGFunction
        go xs ys [] = DictConstructor o (reverse xs ++ reverse ys)
        go xs ys (Required _ t n :: zs) =
          go (Required (MkArgName n.value) (tpe e t) :: xs) ys zs
        go xs ys (Optional t n d :: zs) =
          go xs (OptionalArg (MkArgName n.value) (tpe e t) d :: ys) zs

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
              cgt = Optional (tpe e t) (Just def)
              ak  = kind e d.name
           in [ AttributeGet an ak cgt
              , AttributeSet an ak (optArg e t def)
              ]

mixinFuns : Env -> Domain -> Mixin -> CodegenV (List CGFunction)
mixinFuns e dom m = concat <$> traverse (fromMember . snd) m.members
  where fromMember : MixinMember -> CodegenV (List CGFunction)
        fromMember (MConst _)   = Valid Nil
        fromMember (MOp op)     = fromRegular e dom m.name op
        fromMember (MStr _)     = Valid Nil
        fromMember (MAttrRO ro) = fromAttrRO e m.name ro
        fromMember (MAttr at)   = fromAttr e m.name at

longIndex : IdlTypeF a b
longIndex = D $ NotNull $ P $ Unsigned Long

stringIndex : IdlTypeF a b
stringIndex = D $ NotNull $ S DOMString

isIndex : Eq a => Eq b => IdlTypeF a b -> Bool
isIndex t = t == longIndex || t == stringIndex

ifaceFuns : Env -> Domain -> Interface -> CodegenV (List CGFunction)
ifaceFuns e dom i = concat <$> traverse (fromMember . snd) i.members
  where getter :  Maybe OperationName
               -> IdlType
               -> ArgumentList
               -> CodegenV (List CGFunction)
        getter op t (NoVarArg [a] Nil) =
          if isIndex a.type
             then Valid [Getter op (kind e i.name) (arg e a) (rtpe e t)]
             else Invalid [InvalidGetter dom i.name]
        getter _ _ _ = Invalid [InvalidGetter dom i.name]

        fromMember : InterfaceMember -> CodegenV (List CGFunction)
        fromMember (Z $ MkConstructor args) = fromConstructor e i.name args
        fromMember (S $ Z $ IConst x)       = Valid Nil

        fromMember (S $ Z $ IOp x)          =
          case x of
               MkOp Nothing t n args => fromRegular e dom i.name
                                     $  MkOp () t n args
               MkOp (Just Getter) _ _ _   => ?foo_1 --Valid Nil
               MkOp (Just Setter) _ _ _   => ?foo_2 --Valid Nil
               MkOp (Just Deleter) _ _ _   => Valid Nil

        fromMember (S $ Z $ IStr x)        = Valid Nil
        fromMember (S $ Z $ IStatic x)     = Valid Nil
        fromMember (S $ Z $ IAttr x)       = fromAttr e i.name x
        fromMember (S $ Z $ IMap x)        = Valid Nil
        fromMember (S $ Z $ ISet x)        = Valid Nil
        fromMember (S $ Z $ IAttrRO x)     = fromAttrRO e i.name x
        fromMember (S $ Z $ IMapRO x)      = Valid Nil
        fromMember (S $ Z $ ISetRO x)      = Valid Nil
        fromMember (S $ Z $ IAttrInh x)    = Valid Nil
        fromMember (S $ Z $ IIterable x y) = Valid Nil
        fromMember (S $ Z $ IAsync x y xs) = Valid Nil
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

--------------------------------------------------------------------------------
--          Domain
--------------------------------------------------------------------------------

public export
record CGDict where
  constructor MkDict
  name      : Identifier
  super     : Supertypes
  functions : List CGFunction

public export
record CGIface where
  constructor MkIface
  name      : Identifier
  super     : Supertypes
  constants : List Const
  functions : List CGFunction

public export
record CGMixin where
  constructor MkMixin
  name      : Identifier
  constants : List Const
  functions : List CGFunction

public export
record CGCallback where
  constructor MkCallback
  name      : Identifier
  constants : List Const
  type      : IdlType
  args      : ArgumentList

public export
record CGDomain where
  constructor MkDomain
  name      : String
  callbacks : List CGCallback
  dicts     : List CGDict
  enums     : List Enum
  ifaces    : List CGIface
  mixins    : List CGMixin

export
domainFunctions : CGDomain -> List CGFunction
domainFunctions d =  (d.dicts  >>= functions)
                  ++ (d.ifaces >>= functions)
                  ++ (d.mixins >>= functions)

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
