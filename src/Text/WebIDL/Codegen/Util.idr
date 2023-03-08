module Text.WebIDL.Codegen.Util

import Data.List
import Data.Nat
import Data.Stream
import public Data.String
import public Data.Vect
import public Text.PrettyPrint.Bernardy
import public Text.WebIDL.Codegen.Types
import public Text.WebIDL.Types

%default total

export
mapFirstChar : (Char -> Char) -> String -> String
mapFirstChar f x = case unpack x of
  []     => ""
  h :: t => pack (f h :: t)

--------------------------------------------------------------------------------
--          Sorted Lists
--------------------------------------------------------------------------------

export
sortedNubOn : Ord b => (a -> b) -> List a -> List a
sortedNubOn f = nub . sortBy (comparing f)
  where
    nub : List a -> List a
    nub (x :: t@(y :: ys)) = if f x == f y then nub t else x :: nub t
    nub xs                 = xs

--------------------------------------------------------------------------------
--          Modules
--------------------------------------------------------------------------------

export
moduleName : String -> String
moduleName "uievents" = "UIEvents"
moduleName s          = mapFirstChar toUpper s

--------------------------------------------------------------------------------
--          String Literals
--------------------------------------------------------------------------------

export
unquote : String -> List Char
unquote = run . unpack
  where
    run : List Char -> List Char
    run []                   = []
    run ('\\' :: '"' :: cs)  = '"' :: run cs
    run ('"' :: cs)          = run cs
    run (c   :: cs)          = c :: run cs

||| Generates a data constructor from a string literal.
||| This is used for enums, where some values are not
||| valid idris identifiers. Some necessary adjustments
||| are hardcoded here.
export
toDataConstructor : String -> String
toDataConstructor s = case unquote s of
  []        => "Empty"
  ['2','d'] => "TwoD"
  c :: cs   => pack (toUpper c :: run cs)

  where
    run : List Char -> List Char
    run []             = []
    run (x :: xs@(c :: cs)) =
      if isAlphaNum x then x :: run xs else toUpper c :: run cs
    run (c :: cs)      = c :: run cs

--------------------------------------------------------------------------------
--          Comments
--------------------------------------------------------------------------------

export
title : String -> String
title n = """
  --------------------------------------------------------------------------------
  --          \{n}
  --------------------------------------------------------------------------------
  """

export
section : String -> List String -> String
section _ Nil = ""
section t ds  = unlines ("" :: title t :: ds)

--------------------------------------------------------------------------------
--          Namespaces Implementations
--------------------------------------------------------------------------------

export
namespaced : Identifier -> List String -> String
namespaced _ [] = ""
namespaced n ds = unlines $ "" :: "namespace \{n.value}" :: ds

--------------------------------------------------------------------------------
--          Generating Functions
--------------------------------------------------------------------------------

parameters {opts : LayoutOpts}

  export
  functionTypeOnly : (res : Doc opts) -> (args : List $ Doc opts) -> Doc opts
  functionTypeOnly res []     = parens $ "() ->" <++> res
  functionTypeOnly res (h::t) =
    let withArrows := map (line "->" <++>) (t ++ [res])
        sl         := parens (hsep $ h :: withArrows)
        ml         := vsep $ (line "(  " <+> h) :: withArrows ++ [line ")"]
     in ifMultiline sl ml

  export
  functionType :
       (name : IdrisIdent)
    -> (sep  : String)
    -> (res  : Doc opts)
    -> (args : List $ Doc opts)
    -> Doc opts
  functionType nm sep res [] =
    let head := line "\{nm} \{sep}"
     in ifMultiline (head <++> res) (vappend head $ indent 2 res)
  functionType nm sep res (h::t) =
    let head       := line "\{nm} \{sep}"
        withArrows := map (line "->" <++>) (t ++ [res])
        sl         := hsep $ head :: h :: withArrows
        ml         := vsep $ indent 3 h :: withArrows
     in ifMultiline sl (vappend head $ indent 2 ml)

  export
  typeDecl :
       (name : IdrisIdent)
    -> (res : Doc opts)
    -> (args : List $ Doc opts)
    -> Doc opts
  typeDecl n = functionType n ":"
--
--------------------------------------------------------------------------------
--          Function Names
--------------------------------------------------------------------------------

ix : Nat -> String
ix Z = ""
ix k = show k

export
primSetter : Nat -> IdrisIdent
primSetter k = Prim . fromString $ "set" ++ ix k

export
setter : Nat -> IdrisIdent
setter k = fromString $ "set" ++ ix k

export
primGetter : Nat -> IdrisIdent
primGetter k = Prim . fromString $ "get" ++ ix k

export
getter : Nat -> IdrisIdent
getter k = fromString $ "get" ++ ix k

export
primAttrSetter : Nat -> AttributeName -> IdrisIdent
primAttrSetter k n =
  Prim $ fromString ("set" ++ mapFirstChar toUpper n.value ++ ix k)

export
attrSetter : Nat -> AttributeName -> IdrisIdent
attrSetter k n = fromString $ "set" ++ mapFirstChar toUpper n.value ++ ix k

export
primAttrGetter : Nat -> AttributeName -> IdrisIdent
primAttrGetter k n = Prim $ fromString (n.value ++ ix k)

export
attrGetter : Nat -> AttributeName -> IdrisIdent
attrGetter k n = fromString $ n.value ++ ix k

export
primOp : Nat -> OperationName -> IdrisIdent
primOp k n = Prim $ fromString (n.value ++ ix k)

export
op : Nat -> OperationName -> IdrisIdent
op k n = fromString (n.value ++ ix k)

export
primConstr : Nat -> IdrisIdent
primConstr k = Prim $ fromString ("new" ++ ix k)

export
constr : Nat -> IdrisIdent
constr k = fromString ("new" ++ ix k)

export
marshallCallback : Identifier -> IdrisIdent
marshallCallback i = fromString $ "to" ++ i.value

export
primMarshallCallback : Identifier -> IdrisIdent
primMarshallCallback i = Prim . fromString $ "to" ++ i.value

--------------------------------------------------------------------------------
--          Foreign Function Implementations
--------------------------------------------------------------------------------

export
argNames : Stream String
argNames =
  "a" :: "b" :: "c" :: "d" :: "e" :: "f" :: "g" ::
  "h" :: "i" :: "j" :: "k" :: "l" :: "m" :: "n" ::
  "o" :: "p" :: "q" :: "r" :: "s" :: "t" :: "u" ::
  "v" :: "w" :: "y" :: "z" ::
  map (\v => "x" ++ show {ty = Integer} v) [1 ..]

export
unShadowingArgNames : IdrisIdent -> Stream String
unShadowingArgNames i = go "\{i}" argNames
  where
    go : String -> Stream String -> Stream String
    go s (h :: t) = if s == h then t else h :: go s t

foreignBrowser : String -> String
foreignBrowser s = "%foreign \"browser:lambda:\{s}\""

export
attrGetFFI : AttributeName -> String
attrGetFFI n = foreignBrowser "x=>x.\{n.value}"

export
staticAttrGetFFI : Kind -> AttributeName -> String
staticAttrGetFFI o n =
  foreignBrowser "()=>\{kindToString o}.\{n.value}"

export
attrSetFFI : AttributeName -> String
attrSetFFI n = foreignBrowser "(x,v)=>{x.\{n.value} = v}"

export
staticAttrSetFFI : Kind -> AttributeName -> String
staticAttrSetFFI o n =
  foreignBrowser "v=>{\{kindToString o}.\{n.value} = v}"

export
funFFI : OperationName -> Nat -> String
funFFI n Z = foreignBrowser "x=>x.\{n.value}()"
funFFI n k =
  let vs = take k argNames
      vals = fastConcat $ intersperse "," vs
   in foreignBrowser "(x,\{vals})=>x.\{n.value}(\{vals})"

export
funFFIVarArg : OperationName -> Nat -> String
funFFIVarArg n k =
  let vs = take (pred k) argNames
      vals = fastConcat $ intersperse "," (vs ++ ["va"])
      args = fastConcat $ intersperse "," (vs ++ ["...va()"])
   in foreignBrowser "(x,\{vals})=>x.\{n.value}(\{args})"

export
staticFunFFI : Kind -> OperationName -> Nat -> String
staticFunFFI o n Z = foreignBrowser "x=>x.\{n.value}()"
staticFunFFI o n k =
  let vs = take k argNames
      vals = fastConcat $ intersperse "," vs
   in foreignBrowser "(\{vals})=>\{kindToString o}.\{n.value}(\{vals})"

export
staticFunFFIVarArg : Kind -> OperationName -> Nat -> String
staticFunFFIVarArg o n k =
  let vs = take (pred k) argNames
      vals = fastConcat $ intersperse "," (vs ++ ["va"])
      args = fastConcat $ intersperse "," (vs ++ ["...va()"])
   in foreignBrowser "(\{vals})=>\{kindToString o}.\{n.value}(\{args})"

export
conFFI : Kind -> Nat -> String
conFFI n k =
  let vs = take k argNames
      vals = fastConcat $ intersperse "," vs
   in foreignBrowser "(\{vals})=> new \{kindToString n}(\{vals})"

export
conFFIVarArg : Kind -> Nat -> String
conFFIVarArg n k =
  let vs = take (pred k) argNames
      vals = fastConcat $ intersperse "," (vs ++ ["va"])
      args = fastConcat $ intersperse "," (vs ++ ["...va()"])
   in foreignBrowser "(\{vals})=> new \{kindToString n}(\{args})"

export
dictConFFI : List ArgumentName -> String
dictConFFI ns =
  let vs     = take (length ns) argNames
      vals   = fastConcat $ intersperse "," vs
      fields = fastConcat $ intersperse "," (zipWith app vs ns)
   in foreignBrowser "(\{vals})=> ({\{fields}})"

  where app : String -> ArgumentName -> String
        app v a = a.value ++ ": " ++ v

export
getterFFI : String
getterFFI = foreignBrowser "(o,x)=>o[x]"

export
setterFFI : String
setterFFI = foreignBrowser "(o,x,v)=>o[x] = v"

export
callbackFFI : Nat -> String
callbackFFI n =
  let vs = fastConcat $ intersperse "," $ take n argNames
   in foreignBrowser "x=>(\{vs})=>x(\{vs})()"

--------------------------------------------------------------------------------
--          Pretty Printing
--------------------------------------------------------------------------------

export %inline
render80 : Doc (Opts 80) -> String
render80 = render _
