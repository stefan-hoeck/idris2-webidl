module Text.WebIDL.Codegen.Rules

import Data.List
import Data.Validated
import Data.SortedMap
import Text.WebIDL.Types

%default total

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

||| Env
public export
record Env where
  constructor MkEnv
  types         : JSTypes
  maxIterations : Nat
  callbacks     : List Identifier

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

covering export
isCallback : List Identifier -> IdlType -> Bool
isCallback cbs = any (`elem` cbs) . typeIdentifiers

covering
callbacks : List Domain -> List Identifier
callbacks ds = let cs =  (ds >>= map name . callbacks)
                      ++ (ds >>= map name . callbackInterfaces)

                   ts = map name
                      . filter (isCallback cs . type)
                      $ ds >>= typedefs

                in cs ++ ts

covering export
env : (maxInheritance : Nat) -> List Domain -> Env
env mi ds = MkEnv (jsTypes ds) mi (callbacks ds)

||| The parent types and mixins of a type. This is
||| used by the code generator to implement the
||| `JS.Inheritance.JSType` instances.
public export
record Supertypes where
  constructor MkSupertypes
  parents : List Identifier
  mixins  : List Identifier

objectOnly : Supertypes
objectOnly = MkSupertypes [MkIdent "JSObject"] []

||| Calculates the supertypes and mixins for a given
||| identifier.
|||
|||  @maxIterations : Maximal number of iterations. Without this,
|||                   the algorithm might loop forever in case of
|||                   cyclic dependencies. This value corresponds
|||                   to the maximal length of the inheritance chain.
export
supertypes : JSTypes -> (maxIterations : Nat) -> Identifier -> Supertypes
supertypes _   0    i = objectOnly
supertypes js (S k) i =
  case lookup i js of
       Nothing                              => objectOnly

       Just $ MkJSType Nothing mixins       =>
         record { mixins = mixins } objectOnly

       Just $ MkJSType (Just parent) mixins =>
         let MkSupertypes parents mixins2 = supertypes js k parent
          in MkSupertypes (parent :: parents) (mixins ++ mixins2)

--------------------------------------------------------------------------------
--          Codegen Errors
--------------------------------------------------------------------------------

public export
data CodegenErr : Type where
  MandatoryAfterOptional : Domain -> IdrisIdent -> CodegenErr
  RegularOpWithoutName   : Domain -> CodegenErr
  VarargAndOptionalArgs  : Domain -> IdrisIdent -> CodegenErr
  VarargNotLastArg       : Domain -> IdrisIdent -> CodegenErr
  VarargConstructor      : Domain -> Identifier -> CodegenErr

public export
Codegen : Type -> Type
Codegen = Either (List CodegenErr)

public export
CodegenV : Type -> Type
CodegenV = Validated (List CodegenErr)

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

||| A function argument in the code generator.
public export
record Arg where
  constructor MkArg
  name : IdrisIdent
  type : IdlType

export
arg : ArgumentName -> IdlType -> Arg
arg n t = MkArg (fromString n.value) t

||| A function, for which we will generate some code.
public export
data CodegenFunction : Type where
  ||| An attribute setter.
  AttributeSet :  (name : IdrisIdent)
               -> (obj : Identifier)
               -> (tpe : IdlType)
               -> CodegenFunction

  ||| An attribute getter.
  AttributeGet :  (name : IdrisIdent)
               -> (obj : Identifier)
               -> (tpe : IdlType)
               -> CodegenFunction

  ||| A setter for an optional attribute.
  OptionalAttributeSet :  (name : IdrisIdent)
                       -> (obj : Identifier)
                       -> (tpe : IdlType)
                       -> CodegenFunction

  ||| A getter for an optional attribute.
  OptionalAttributeGet :  (name  : IdrisIdent)
                       -> (obj : Identifier)
                       -> (tpe   : IdlType)
                       -> (deflt : Default)
                       -> CodegenFunction

  ||| An interface constructor with (possibly) optional arguments.
  Constructor      :  (name         : Identifier)
                   -> (args         : List Arg)
                   -> (optionalArgs : List Arg)
                   -> CodegenFunction

  ||| A regular function with (possibly) optional arguments.
  Regular      :  (name         : IdrisIdent)
               -> (args         : List Arg)
               -> (optionalArgs : List Arg)
               -> (returnType   : IdlType)
               -> CodegenFunction

  ||| A regular function with a terminal vararg.
  VarArg       :  (name         : IdrisIdent)
               -> (args         : List Arg)
               -> (varArg       : Arg)
               -> (returnType   : IdlType)
               -> CodegenFunction

||| Extract the name of a function
export
name : CodegenFunction -> IdrisIdent
name (AttributeSet n _ _)           = n
name (AttributeGet n _ _)           = n
name (OptionalAttributeSet n _ _)   = n
name (OptionalAttributeGet n _ _ _) = n
name (Constructor _ _ _)            = II "new" Refl
name (Regular n _ _ _)              = n
name (VarArg n _ _ _)               = n

||| Extract the type of a function
export
type : CodegenFunction -> IdlType
type (AttributeSet _ _ t)            = t
type (AttributeGet _ _ t)            = t
type (OptionalAttributeSet _ _ t)    = t
type (OptionalAttributeGet _ _ t _)  = t
type (Constructor n _ _)             = identToType n
type (Regular _ _ _ t)               = t
type (VarArg _ _ _ t)                = t

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
priority : CodegenFunction -> (Nat,String,Nat)
priority (Constructor n _ _)            = (0,n.value,0)
priority (AttributeSet n _ _)           = (1,show n,1)
priority (AttributeGet n _ _)           = (1,show n,0)
priority (OptionalAttributeSet n _ _)   = (1,show n,1)
priority (OptionalAttributeGet n _ _ _) = (1,show n,0)
priority (Regular n _ _ _)              = (2,show n,0)
priority (VarArg n _ _ _)               = (2,show n,0)

-- (mandatory args, vararg or optional args)
SepArgs : Type
SepArgs = (List Arg, Either Arg (List Arg))

-- The following rules apply:
--
--  * a regular operation's name must not be `Nothing`
--  * there can only be one vararg and it must be the last
--    argument
--  * there must be no mandatory argument after an optional
--    argument
--  * optional arguments and varargs must not be mixed
fromArgList : Domain -> IdrisIdent -> IdlType -> ArgumentList -> Codegen SepArgs
fromArgList dom on t args =
   case run args of
        Left x                => Left x
        Right (as,os,Nothing) => Right (as, Right os)
        Right (as,Nil,Just a) => Right (as, Left a)
        Right (as,_,Just _)   => Left [VarargAndOptionalArgs dom on]

  where run :  ArgumentList -> Codegen (List Arg, List Arg, Maybe Arg)
        run []                              = Right (Nil,Nil,Nothing)
        run ((_, VarArg t n)     :: Nil)    = Right (Nil,Nil,Just $ arg n t)
        run ((_, VarArg t n)     :: _)      = Left [VarargNotLastArg dom on]
        run ((_, Optional (_,t) n d) :: xs) =
          do (Nil,os,va) <- run xs
               | _ => Left [MandatoryAfterOptional dom on]
             pure (Nil, arg n t :: os, va)

        run ((_, Mandatory t n)  :: xs)     =
          map (\(as,os,m) => (arg n t :: as, os, m)) (run xs)

fromRegular : Domain -> RegularOperation -> CodegenV (List CodegenFunction)
fromRegular dom (MkOp () t Nothing args) =
  Invalid [RegularOpWithoutName dom]

fromRegular dom (MkOp () t (Just op) args) = 
  let on = the IdrisIdent $ fromString op.value
   in case fromArgList dom on t args of
           Left x               => Invalid x
           Right (as, Left a)   => Valid [ VarArg on as a t ]
           Right (as, Right os) => Valid [ Regular on as os t ]

fromConstructor :  Domain
                -> Identifier
                -> ArgumentList
                -> CodegenV (List CodegenFunction)
fromConstructor dom ident args =
  let con = II "new" Refl
   in case fromArgList dom con (identToType ident) args of
           Left x  => Invalid x
           Right (as, Left a)   => Invalid [VarargConstructor dom ident]
           Right (as, Right os) => Valid [ Constructor ident as os ]

fromAttrRO : Identifier -> Readonly Attribute -> CodegenV (List CodegenFunction)
fromAttrRO obj (MkRO $ MkAttribute _ t n) =
          Valid [AttributeGet (fromString n.value) obj t]

fromAttr : Identifier -> Attribute -> CodegenV (List CodegenFunction)
fromAttr obj (MkAttribute _ t n) =
  let ii = fromString n.value
   in Valid [AttributeGet ii obj t, AttributeSet ii obj t]


||| Tries to extract all functions from a dictionary
||| definition.
export
fromDictionary : Dictionary -> CodegenV (List CodegenFunction)
fromDictionary d = Valid $ d.members >>= fromMember . snd
  where fromMember : DictionaryMemberRest -> List CodegenFunction
        fromMember (Required _ t n) =
          let ii = fromIdent n
           in [AttributeGet ii d.name t, AttributeSet ii d.name t]
        fromMember (Optional t n def) =
          let ii = fromIdent n
           in [ OptionalAttributeGet ii d.name t def
              , OptionalAttributeSet ii d.name t
              ]

||| Tries to extract all functions from a mixin definition.
export
fromMixin : Domain -> Mixin -> CodegenV (List CodegenFunction)
fromMixin dom m = concat <$> traverse (fromMember . snd) m.members
  where fromMember : MixinMember -> CodegenV (List CodegenFunction)
        fromMember (MConst _)   = Valid Nil
        fromMember (MOp op)     = fromRegular dom op
        fromMember (MStr _)     = Valid Nil
        fromMember (MAttrRO ro) = fromAttrRO m.name ro
        fromMember (MAttr at)   = fromAttr m.name at

||| Tries to extract all functions from an interface definition.
export
fromInterface : Domain -> Interface -> CodegenV (List CodegenFunction)
fromInterface dom i = concat <$> traverse (fromMember . snd) i.members
  where fromMember : InterfaceMember -> CodegenV (List CodegenFunction)
        fromMember (Z $ MkConstructor args) = fromConstructor dom i.name args
        fromMember (S $ Z $ IConst x)       = Valid Nil

        fromMember (S $ Z $ IOp x)          =
          case x of
               MkOp Nothing t n args => fromRegular dom $ MkOp () t n args
               MkOp (Just _) _ _ _   => Valid Nil

        fromMember (S $ Z $ IStr x)        = Valid Nil
        fromMember (S $ Z $ IStatic x)     = Valid Nil
        fromMember (S $ Z $ IAttr x)       = fromAttr i.name x
        fromMember (S $ Z $ IMap x)        = Valid Nil
        fromMember (S $ Z $ ISet x)        = Valid Nil
        fromMember (S $ Z $ IAttrRO x)     = fromAttrRO i.name x
        fromMember (S $ Z $ IMapRO x)      = Valid Nil
        fromMember (S $ Z $ ISetRO x)      = Valid Nil
        fromMember (S $ Z $ IAttrInh x)    = Valid Nil
        fromMember (S $ Z $ IIterable x y) = Valid Nil
        fromMember (S $ Z $ IAsync x y xs) = Valid Nil
        fromMember (S $ S x) impossible
