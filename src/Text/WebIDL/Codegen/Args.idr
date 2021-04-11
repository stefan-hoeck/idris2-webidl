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

public export
record PrettyType where
  constructor MkPrettyType
  ffi      : Doc ()
  api      : Doc ()
  sameType : Bool

same : Doc () -> PrettyType
same d = MkPrettyType d d True

diff : Doc () -> Doc () -> PrettyType
diff f a = MkPrettyType f a False

public export
record PrettyArg where
  constructor MkPrettyArg
  name     : ArgumentName
  ffi      : Doc ()
  api      : Doc ()
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
kind _ (KEnum y) = diff "String" (pretty $ y.value)
kind _ k         = same . pretty . value $ ident k

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

buff : Prec -> BufferRelatedType -> PrettyType
buff _ Uint8Array        = same "UInt8Array"
buff _ Uint16Array       = same "UInt8Array"
buff _ Uint32Array       = same "UInt8Array"
buff _ Uint8ClampedArray = same "UInt8ClampedArray"
buff _ x                 = same . pretty $ show x

-- booleans are marshalled from Idris2 `Bool` to JS `Boolean`
-- and back
prim : Prec -> PrimitiveType -> PrettyType
prim _ Boolean             = diff "Boolean" "Bool"
prim _ (Unsigned Short)    = same "UInt16"
prim _ (Unsigned Long)     = same "UInt32"
prim _ (Unsigned LongLong) = same "UInt64"
prim _ (Signed Short)      = same "Int16"
prim _ (Signed Long)       = same "Int32"
prim _ (Signed LongLong)   = same "Int64"
prim _ (Unrestricted x)    = same "Double"
prim _ (Restricted x)      = same "Double"
prim _ Undefined           = same "Undefined"
prim _ Byte                = same "Int8"
prim _ Octet               = same "UInt8"
prim _ BigInt              = same "Integer"

string : Prec -> StringType -> PrettyType
string _ ByteString = same "ByteString"
string _ DOMString  = same "String"
string _ USVString  = same "String"

nullable :  (Prec -> a -> PrettyType) -> Prec -> Nullable a -> PrettyType
nullable f p (MaybeNull x) = 
  let MkPrettyType ffi api _ = f App x
   in diff (prettyCon p "Nullable" [ffi]) (prettyCon p "Maybe" [api])
nullable f p (NotNull x)   = f p x

mutual
  -- types wrapped in `Sequence` or similar are
  -- kept in their FFI form.
  dist : Prec -> CGDist -> PrettyType
  dist _ Object                = same "Object"
  dist _ Symbol                = same "Symbol"
  dist p (P x)                 = prim p x
  dist p (S x)                 = string p x
  dist p (I x)                 = kind p x
  dist p (B x)                 = buff p x
  dist p (Sequence _ x)        = same $ prettyCon p "Array" [ffi $ idl App x]
  dist p (FrozenArray _ x)     = same $ prettyCon p "Array" [ffi $ idl App x]
  dist p (ObservableArray _ x) = same $ prettyCon p "Array" [ffi $ idl App x]
  dist p (Record s _ x) =
    same $ prettyCon p "Record" [ffi $ string App s, ffi $ idl App x]

  un : Prec -> CGUnion -> PrettyType
  un p (UT fst snd rest) =
    let -- ffi repr
        argsFFI = map (ffi . um App) (fst :: snd :: rest)
        conFFI  = "Union" ++ show (length argsFFI)

        -- api repr
        fstAPI  = api $ um Open fst
        argsAPI = map (api . um Open) (snd :: rest)
        brkt    = align . sep $  ["[" <++> fstAPI]
                              ++ map ("," <++>) argsAPI
                              ++ ["]"]

     in diff (prettyCon p (pretty conFFI) argsFFI)
             (prettyCon p "NS I" [brkt])

  um : Prec -> CGMember -> PrettyType
  um p (MkUnionMember _ t) = dist p t

  export
  idl : Prec -> CGType -> PrettyType
  idl p Any         = same "AnyPtr"
  idl p (D x)       = nullable dist p x
  idl p (U x)       = nullable un p x
  idl p (Promise x) = same $ prettyCon p "Promise" [ffi $ idl App x]

export
constTpe : Prec -> ConstTypeF Identifier -> Doc ()
constTpe p (CP x) = api $ prim p x
constTpe p (CI x) = pretty $ x.value

atype : Prec -> AType -> PrettyType
atype p = idl p . type

returnType : ReturnType -> PrettyType
returnType Undefined     = MkPrettyType "PrimIO ()" "JSIO ()" True
returnType (UndefOr x _) = 
  let MkPrettyType ffi api _ = atype App x
   in diff ("PrimIO $ UndefOr" <++> ffi) ("JSIO $ Optional" <++> api)

returnType (FromIdl x)   =
  let MkPrettyType ffi api b = atype App x
   in MkPrettyType ("PrimIO" <++> ffi) ("JSIO" <++> api) b

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

argType : CGArg -> PrettyType
argType (Mandatory _ t)  = atype Open t
argType (VarArg _ t)     = same $ "VarArg"  <++> (ffi $ atype App t)
argType (Optional _ t _) = 
  let MkPrettyType ffi api _ = atype App t
   in diff ("UndefOr" <++> ffi) ("Optional" <++> api)

arg : PrettyArg -> Doc ()
arg a = parens $ hsep [pretty (argIdent a), ":", a.api]

prettyArg : CGArg -> PrettyArg
prettyArg a = let MkPrettyType ffi arg same = argType a
               in MkPrettyArg (argName a) ffi arg same

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

funTypeFFI : (name : IdrisIdent) -> ReturnType -> Args -> Doc ()
funTypeFFI n t as = typeDecl n (ffi $ returnType t) (map (ffi . prettyArg) as)

funType : (name : IdrisIdent) -> PrettyType -> PrettyArgs -> Doc ()
funType n t as = typeDecl n t.api (map arg as)

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
        primCall (MkPrettyType _ _ True)  = "primJS $"
        primCall (MkPrettyType _ _ False) = "tryJS" <++> nameNS <++> "$"

        adjVal : String -> PrettyArg -> Doc ()
        adjVal v (MkPrettyArg _ _ _ True)  = pretty v
        adjVal v (MkPrettyArg _ _ _ False) = parens ("toFFI" <++> pretty v)
