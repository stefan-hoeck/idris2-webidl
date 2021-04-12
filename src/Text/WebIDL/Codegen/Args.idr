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
import Text.WebIDL.Codegen.Types
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types

||| Prettyfied types in the codegen plus two boolean flags,
||| indicating, whether the two types are identical (and therefore,
||| there is no need for marshalling from or to the FFI),
||| and whether the type has a `SafeCast` instance (only if this
||| is set to true are we allowed to marshall a return type
||| from the FFI).
public export
record PrettyType where
  constructor MkPrettyType
  ffi      : Doc ()
  api      : Doc ()
  ret      : Doc ()
  sameType : Bool
  safeCast : Bool

sameTrue : Doc () -> PrettyType
sameTrue d = MkPrettyType d d d True True

sameFalse : Doc () -> PrettyType
sameFalse d = MkPrettyType d d d True False

diffTrue : Doc () -> Doc () -> PrettyType
diffTrue d a = MkPrettyType d a a False True

diffFalse : Doc () -> Doc () -> Doc () -> PrettyType
diffFalse f a r = MkPrettyType f a r False False

public export
record PrettyArg where
  constructor MkPrettyArg
  name     : ArgumentName
  ffi      : Doc ()
  api      : Doc ()
  ret      : Doc ()
  sameType : Bool

export
argIdent : PrettyArg -> IdrisIdent
argIdent = fromString . value . name

public export
PrettyArgs : Type
PrettyArgs = List PrettyArg

--------------------------------------------------------------------------------
--          Kinds
--------------------------------------------------------------------------------

kind : Prec -> Kind -> PrettyType
kind _ (KAlias y)      = sameFalse (pretty $ y.value)
kind _ (KCallback y)   = sameFalse (pretty $ y.value)
kind _ (KDictionary y) = sameTrue (pretty $ y.value)
kind _ (KEnum y)       = diffTrue "String" (pretty $ y.value)
kind _ (KInterface y)  = sameTrue (pretty $ y.value)
kind _ (KMixin y)      = sameFalse (pretty $ y.value)
kind _ (KOther y)      = sameFalse (pretty $ y.value)

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

buff : Prec -> BufferRelatedType -> PrettyType
buff _ Uint8Array        = sameFalse "UInt8Array"
buff _ Uint16Array       = sameFalse "UInt8Array"
buff _ Uint32Array       = sameFalse "UInt8Array"
buff _ Uint8ClampedArray = sameFalse "UInt8ClampedArray"
buff _ x                 = sameFalse . pretty $ show x

-- booleans are marshalled from Idris2 `Bool` to JS `Boolean`
-- and back
prim : Prec -> PrimitiveType -> PrettyType
prim _ Boolean             = diffTrue "Boolean" "Bool"
prim _ (Unsigned Short)    = sameTrue "UInt16"
prim _ (Unsigned Long)     = sameTrue "UInt32"
prim _ (Unsigned LongLong) = sameTrue "UInt64"
prim _ (Signed Short)      = sameTrue "Int16"
prim _ (Signed Long)       = sameTrue "Int32"
prim _ (Signed LongLong)   = sameTrue "Int64"
prim _ (Unrestricted x)    = sameTrue "Double"
prim _ (Restricted x)      = sameTrue "Double"
prim _ Undefined           = sameTrue "Undefined"
prim _ Byte                = sameTrue "Int8"
prim _ Octet               = sameTrue "UInt8"
prim _ BigInt              = sameTrue "Integer"

string : Prec -> StringType -> PrettyType
string _ ByteString = sameFalse "ByteString"
string _ DOMString  = sameTrue "String"
string _ USVString  = sameTrue "String"

nullable :  (Prec -> a -> PrettyType) -> Prec -> Nullable a -> PrettyType
nullable f p (MaybeNull x) = 
  let MkPrettyType ffi api ret _ b = f App x
   in if b 
         then diffTrue (prettyCon p "Nullable" [ffi])
                       (prettyCon p "Maybe" [api])
         else diffFalse (prettyCon p "Nullable" [ffi])
                        (prettyCon p "Maybe" [api])
                        (prettyCon p "Maybe" [ret])
nullable f p (NotNull x)   = f p x

mutual
  -- types wrapped in `Sequence` or similar are
  -- kept in their FFI form.
  dist : Prec -> CGDist -> PrettyType
  dist _ Object                = sameTrue "Object"
  dist _ Symbol                = sameTrue "Symbol"
  dist p (P x)                 = prim p x
  dist p (S x)                 = string p x
  dist p (I x)                 = kind p x
  dist p (B x)                 = buff p x
  dist p (Sequence _ x)        = sameFalse $ prettyCon p "Array" [ffi $ idl App x]
  dist p (FrozenArray _ x)     = sameFalse $ prettyCon p "Array" [ffi $ idl App x]
  dist p (ObservableArray _ x) = sameFalse $ prettyCon p "Array" [ffi $ idl App x]
  dist p (Record s _ x) =
    sameFalse $ prettyCon p "Record" [ffi $ string App s, ffi $ idl App x]

  un : Prec -> CGUnion -> PrettyType
  un p (UT fst snd rest) =
    let -- ffi repr
        args    = map (um App) (fst :: snd :: rest)
        argsFFI = map ffi args
        conFFI  = "Union" ++ show (length argsFFI)

        -- api repr
        fstAPI  = api $ um Open fst
        argsAPI = map (api . um Open) (snd :: rest)
        brkt    = align . sep $  ["[" <++> fstAPI]
                              ++ map ("," <++>) argsAPI
                              ++ ["]"]

        canSafelyCast = all PrettyType.safeCast args
        prettyFFI = prettyCon p (pretty conFFI) argsFFI
        prettyAPI = prettyCon p "NS I" [brkt]

     in if canSafelyCast
           then diffTrue prettyFFI prettyAPI
           else diffFalse prettyFFI prettyAPI prettyFFI

  um : Prec -> CGMember -> PrettyType
  um p (MkUnionMember _ t) = dist p t

  export
  idl : Prec -> CGType -> PrettyType
  idl p Any         = sameFalse "AnyPtr"
  idl p (D x)       = nullable dist p x
  idl p (U x)       = nullable un p x
  idl p (Promise x) = sameFalse $ prettyCon p "Promise" [ffi $ idl App x]

export
constTpe : Prec -> CGConstType -> Doc ()
constTpe p (CP x) = api $ prim p x
constTpe p (CI x) = pretty $ kindToString x

atype : Prec -> AType -> PrettyType
atype p = idl p . type

returnType' : (primIO : Doc ()) -> ReturnType -> PrettyType
returnType' p Undefined     =
  MkPrettyType (p <++> "()") "JSIO ()" "JSIO ()" True True
returnType' p (UndefOr x _) = 
  let MkPrettyType ffi api ret _ b = atype App x
   in if b
         then diffTrue (p <++> "$ UndefOr" <++> ffi)
                       ("JSIO $ Optional" <++> api)
         else diffFalse (p <++> "$ UndefOr" <++> ffi)
                        ("JSIO $ Optional" <++> ffi)
                        ("JSIO $ Optional" <++> ret)

returnType' p (FromIdl x)   =
  let MkPrettyType ffi api ret b sc = atype App x
   in MkPrettyType (p <++> ffi) ("JSIO" <++> api) ("JSIO" <++> ret) b sc

returnType : ReturnType -> PrettyType
returnType = returnType' "PrimIO"

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

argType : CGArg -> PrettyType
argType (Mandatory _ t)  = atype Open t
argType (VarArg _ t)     = sameFalse $ "VarArg"  <++> (ffi $ atype App t)
argType (Optional _ t _) = 
  let MkPrettyType ffi api ret _ b = atype App t
   in if b
         then diffTrue ("UndefOr" <++> ffi) ("Optional" <++> api)
         else diffFalse ("UndefOr" <++> ffi)
                        ("Optional" <++> api)
                        ("Optional" <++> ret)

arg : PrettyArg -> Doc ()
arg a = parens $ hsep [pretty (argIdent a), ":", a.api]

prettyArg : CGArg -> PrettyArg
prettyArg a = let MkPrettyType ffi api ret same _ = argType a
               in MkPrettyArg (argName a) ffi api ret same

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

funTypeFFI : (name : IdrisIdent) -> ReturnType -> Args -> Doc ()
funTypeFFI n t as = typeDecl n (ffi $ returnType t) (map (ffi . prettyArg) as)

funType : (name : IdrisIdent) -> PrettyType -> PrettyArgs -> Doc ()
funType n t as = typeDecl n t.ret (map arg as)

export
callbackFFI :  (obj  : Identifier)
            -> (name : IdrisIdent)
            -> (impl : String)
            -> (args : Args)
            -> (tpe  : ReturnType)
            -> String
callbackFFI o n impl as t =
  let cbTpe  = functionTypeOnly (ffi $ returnType' "IO" t)
                                (map (ffi . prettyArg) as)

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
  let cbTpe  = functionTypeOnly (ffi $ returnType' "IO" t)
                                (map (ffi . prettyArg) as)

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
fun :  (ns : Kind)
    -> (name : IdrisIdent)
    -> (prim : IdrisIdent)
    -> Args
    -> ReturnType
    -> String
fun ns name prim as t =
  let -- pretty retturn type
      retType       = returnType t

      -- pretty function arguments
      args          = map prettyArg as

      -- main function
      funTpe        = funType name retType args
      vs            = take (length args) (unShadowingArgNames name)
      appVs         = zipWith adjVal vs args
      primNS        = pretty' (kindToString ns) <+> "." <+> pretty prim
      lhs           = hsep (pretty' name :: map pretty vs)
      rhs           = hsep [primCall retType,primNS,align $ sep appVs]
      funImpl       = ["","export",funTpe,lhs <++> "=" <++> rhs]

      -- function without optional args
      args2        = map prettyArg $ filter (not . isOptional) as
      funTpe2      = funType name2 retType args2
      vs2          = take (length args2) (unShadowingArgNames name2)
      lenDiff      = length args `minus` length args2
      appVs2       = vs2 ++ replicate lenDiff "Undef"
      lhs2         = hsep (pretty' name2 :: map pretty vs2)
      rhs2         = hsep [pretty' name, align (sep $ map pretty appVs2) ]
      funImpl2     = if lenDiff == Z
                        then []
                        else ["","export",funTpe2,lhs2 <++> "=" <++> rhs2]

   in show . indent 2 $ vsep (funImpl ++ funImpl2)

  where name2 : IdrisIdent
        name2 = case name of
                     II v prf     => fromString $ v ++ "'"
                     Prim v       => Prim (v ++ "'")
                     Underscore v => fromString $ v ++ "'"

        nameNS : Doc ()
        nameNS = hcat ["\"",pretty $ kindToString ns,".",pretty name,"\""]

        primCall : PrettyType -> Doc ()
        primCall (MkPrettyType _ _ _ False _) = "tryJS" <++> nameNS <++> "$"
        primCall (MkPrettyType _ _ _ True _)  = "primJS $"

        adjVal : String -> PrettyArg -> Doc ()
        adjVal v (MkPrettyArg _ _ _ _ True)  = pretty v
        adjVal v (MkPrettyArg _ _ _ _ False) = parens ("toFFI" <++> pretty v)
