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
data Location = FFI | API

public export
data UseCase = ArgFFI | ArgAPI | ResFFI | ResAPI

toFFI : UseCase -> UseCase
toFFI ArgFFI = ArgFFI
toFFI ArgAPI = ArgFFI
toFFI ResFFI = ResFFI
toFFI ResAPI = ResFFI

--------------------------------------------------------------------------------
--          Kinds
--------------------------------------------------------------------------------

kind : Prec -> Location -> Kind -> Doc ()
kind _ FFI (KEnum y) = "String"
kind _ x          k  = pretty . value $ ident k

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

buff : Prec -> Location -> BufferRelatedType -> Doc ()
buff _ _ Uint8Array        = "UInt8Array"
buff _ _ Uint16Array       = "UInt8Array"
buff _ _ Uint32Array       = "UInt8Array"
buff _ _ Uint8ClampedArray = "UInt8ClampedArray"
buff _ _ x                 = pretty $ show x

-- booleans are marshalled from Idris2 `Bool` to JS `Boolean`
-- and back
prim : Prec -> Location -> PrimitiveType -> Doc ()
prim _ FFI Boolean                = "Boolean"
prim _ API Boolean                = "Bool"
prim _ _      (Unsigned Short)    = "UInt16"
prim _ _      (Unsigned Long)     = "UInt32"
prim _ _      (Unsigned LongLong) = "UInt64"
prim _ _      (Signed Short)      = "Int16"
prim _ _      (Signed Long)       = "Int32"
prim _ _      (Signed LongLong)   = "Int64"
prim _ _      (Unrestricted x)    = "Double"
prim _ _      (Restricted x)      = "Double"
prim _ _      Undefined           = "Undefined"
prim _ _      Byte                = "Int8"
prim _ _      Octet               = "UInt8"
prim _ _      BigInt              = "Integer"

string : Prec -> Location -> StringType -> Doc ()
string _ _ ByteString = "ByteString"
string _ _ DOMString  = "String"
string _ _ USVString  = "String"

nullable :  (Prec -> Location -> a -> Doc ())
         -> Prec -> Location -> Nullable a -> Doc ()
nullable f p FFI (MaybeNull x) = prettyCon p "Nullable" [f App FFI x]
nullable f p API (MaybeNull x) = prettyCon p "Maybe" [f App API x]
nullable f p lc  (NotNull x)   = f p lc x

mutual
  -- types wrapped in `Sequence` or similar are
  -- kept in their FFI form.
  dist : Prec -> Location -> CGDist -> Doc ()
  dist _ _  Object                = "Object"
  dist _ _  Symbol                = "Symbol"
  dist p lc (P x)                 = prim p lc x
  dist p lc (S x)                 = string p lc x
  dist p lc (I x)                 = kind p lc x
  dist p lc (B x)                 = buff p lc x
  dist p lc (Sequence _ x)        = prettyCon p "Array" [idl App FFI x]
  dist p lc (FrozenArray _ x)     = prettyCon p "Array" [idl App FFI x]
  dist p lc (ObservableArray _ x) = prettyCon p "Array" [idl App FFI x]
  dist p lc (Record s _ x) =
    prettyCon p "Record" [ string App FFI s , idl App FFI x]

  -- Unions are kept in their FFI form unless the are function
  -- arguments, in which case we convert from an `NS I [tps]`
  -- representation to the union.
  un : Prec -> Location -> CGUnion -> Doc ()
  un p API (UT fst snd rest) =
    let afst = um Open API fst
        args = map (um Open API) (snd :: rest)
        brkt = align . sep $ ["[" <++> afst] ++ map ("," <++>) args ++ ["]"]
     in prettyCon p "NS I" [brkt]

  un p lc (UT fst snd rest) =
    let args = map (um App FFI) (fst :: snd :: rest)
        con  = pretty $ "Union" ++ show (length args)
     in prettyCon p con args

  um : Prec -> Location -> CGMember -> Doc ()
  um p lc (UD _ x) = nullable dist p lc x
  um p lc (UU x)   = nullable (\p,_ => un p FFI) p lc x

  export
  idl : Prec -> Location -> CGType -> Doc ()
  idl p lc Any         = "AnyPtr"
  idl p lc (D x)       = nullable dist p lc x
  idl p lc (U x)       = nullable un p lc x
  idl p lc (Promise x) = prettyCon p "Promise" [idl App lc x]

export
constTpe : Prec -> ConstTypeF Identifier -> Doc ()
constTpe p (CP x) = prim p API x
constTpe p (CI x) = pretty $ x.value

atype : Prec -> Location -> AType -> Doc ()
atype p lc = idl p lc . type

returnType : ReturnType -> Doc ()
returnType Undefined     = "JSIO ()"
returnType (UndefOr x _) = "JSIO $ UndefOr" <++> atype App API x
returnType (FromIdl x)   = "JSIO" <++> atype App API x

returnTypeFFI : ReturnType -> Doc ()
returnTypeFFI Undefined     = "PrimIO ()"
returnTypeFFI (UndefOr x _) = "PrimIO $ UndefOr" <++> atype App FFI x
returnTypeFFI (FromIdl x)   = "PrimIO" <++> atype App FFI x

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

argTypeFFI : CGArg -> Doc ()
argTypeFFI (Mandatory _ t)  = atype Open FFI t
argTypeFFI (Optional _ t _) = "UndefOr" <++> atype App FFI t
argTypeFFI (VarArg _ t)     = "VarArg"  <++> atype App FFI t

argType : CGArg -> Doc ()
argType (Mandatory _ t)  = atype Open API t
argType (Optional _ t _) = "UndefOr" <++> atype App API t
argType (VarArg _ t)     = "VarArg"  <++> atype App API t

arg : CGArg -> Doc ()
arg a = parens $ hsep [pretty (argIdent a), ":", argType a]

--------------------------------------------------------------------------------
--          Functions
--------------------------------------------------------------------------------

funTypeFFI : (name : IdrisIdent) -> Args -> ReturnType -> Doc ()
funTypeFFI n as t = typeDecl n (returnTypeFFI t) (map argTypeFFI as)

funType : (name : IdrisIdent) -> (t : ReturnType) -> (args : Args) -> Doc ()
funType n t as = typeDecl n (returnType t) (map arg as)

export
funFFI :  (name : IdrisIdent)
       -> (impl : String)
       -> (args : Args)
       -> (tpe  : ReturnType)
       -> String
funFFI n impl as t =
  let tpe = typeDecl n (returnTypeFFI t) (map argTypeFFI as)
   in show . indent 2 $ vsep ["", "export", pretty impl, tpe ]

export
fun :  (ns : Kind)
    -> (name : IdrisIdent)
    -> (prim : IdrisIdent)
    -> Args
    -> ReturnType
    -> String
fun ns name prim args t =
  let tpe = typeDecl name (returnType t) (map arg args)
   in show . indent 2 $ vsep (["","export",tpe,funImpl] ++ defFun)

  where nonOptArgs : Args
        nonOptArgs = filter (not . isOptional) args

        lenDiff : Nat
        lenDiff = length args `minus` length nonOptArgs

        name' : IdrisIdent
        name' = case name of
                     (II v prf)     => fromString $ v ++ "'"
                     (Prim v)       => Prim (v ++ "'")
                     (Underscore v) => fromString $ v ++ "'"

        primNS : Doc ()
        primNS = pretty (kindToString ns) <+> "." <+> pretty prim

        funImpl : Doc ()
        funImpl = let vs  = take (length args) (unShadowingArgNames name)
                      lhs = hsep (pretty name :: map pretty vs)
                      rhs = hsep [ "primJS $"
                                 , primNS
                                 , align (sep $ map pretty vs)
                                 ]
                   in lhs <++> "=" <++> rhs

        defImpl : Doc ()
        defImpl = let vs  = take (length nonOptArgs) (unShadowingArgNames name)
                      as  = vs ++ replicate lenDiff "undef"
                      lhs = hsep (pretty name' :: map pretty vs)
                      rhs = hsep [ pretty name
                                 , align (sep $ map pretty as)
                                 ]
                   in lhs <++> "=" <++> rhs

        defFun : List $ Doc ()
        defFun = if lenDiff /= Z
                    then 
                      let tpe = typeDecl name' (returnType t) (map arg nonOptArgs)
                       in ["","export",tpe,defImpl]
                    else []
