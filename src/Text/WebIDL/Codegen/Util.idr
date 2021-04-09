module Text.WebIDL.Codegen.Util

import Data.List
import Data.Stream
import public Data.String
import public Data.Vect
import public Text.PrettyPrint.Prettyprinter
import public Text.WebIDL.Codegen.Types
import public Text.WebIDL.Types

export
mapFirstChar : (Char -> Char) -> String -> String
mapFirstChar f x = case fastUnpack x of
                        []       => ""
                        (h :: t) => fastPack (f h :: t)

--------------------------------------------------------------------------------
--          Sorted Lists
--------------------------------------------------------------------------------

export
sortedNubOn : Ord b => (a -> b) -> List a -> List a
sortedNubOn f = nub . sortBy (comparing f)
  where nub : List a -> List a
        nub (x :: t@(y :: ys)) = if f x == f y then nub t else x :: nub t
        nub xs                 = xs

export
prettyList : List (Doc ann) -> Doc ann
prettyList []        = "[]"
prettyList (x :: xs) = align $ sep $  ("[" <++> x) 
                                   :: map ("," <++>) xs
                                   ++ ["]"]

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
unquote = run . fastUnpack
  where run : List Char -> List Char
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
toDataConstructor s =
  case unquote s of
       []        => "Empty"
       ['2','d'] => "TwoD"
       c :: cs => fastPack (toUpper c :: run cs)

  where run : List Char -> List Char
        run []             = []
        run (x :: c :: cs) = 
          if isAlphaNum x then x :: run (c :: cs) else toUpper c :: run cs
        run (c :: cs)      = c :: run cs

--------------------------------------------------------------------------------
--          Comments
--------------------------------------------------------------------------------

export
title : String -> String
title n = #"""
          --------------------------------------------------------------------------------
          --          \#{n}
          --------------------------------------------------------------------------------
          """#

export
section : String -> List String -> String
section _ Nil = ""
section t ds = fastUnlines ("" :: title t :: ds)

--------------------------------------------------------------------------------
--          Namespaces Implementations
--------------------------------------------------------------------------------

export
namespaced : Identifier -> List String -> String
namespaced _ [] = ""
namespaced n ds = fastUnlines $ "" :: #"namespace \#{n.value}"# :: ds

--------------------------------------------------------------------------------
--          Generating Functions
--------------------------------------------------------------------------------

export
functionTypeWithImplicits :  (name : IdrisIdent)
                          -> (sep : Char)
                          -> (res : Doc ())
                          -> (iargs : List $ Doc ())
                          -> (args : List $ Doc ())
                          -> Doc ()
functionTypeWithImplicits n c res [] [] =
  hsep [pretty n, pretty c, res]

functionTypeWithImplicits n c res [] (h :: t) =
  let h' = pretty c <++> flatAlt (" "  <+> h) h
      args = h' :: map ("->" <++>) (t ++ [res])
   in pretty n <++> align (sep args)

functionTypeWithImplicits n c res (h :: t) [] =
  let h' = pretty c <++> flatAlt (" "  <+> h) h
      args = h' :: map ("=>" <++>) (t ++ [res])
   in pretty n <++> align (sep args)

functionTypeWithImplicits n c res (x :: xs) (y :: ys) =
  let x' = pretty c <++> flatAlt (" "  <+> x) x
      args = x' :: map ("=>" <++>) (xs ++ [y])
                ++ map ("->" <++>) (ys ++ [res])

   in pretty n <++> align (sep args)

export
functionType :  (name : IdrisIdent)
             -> (sep : Char)
             -> (res : Doc ())
             -> (args : List $ Doc ())
             -> Doc ()
functionType n c res = functionTypeWithImplicits n c res []

export
typeDeclWithImplicits :  (name : IdrisIdent)
                      -> (res : Doc ())
                      -> (iargs : List $ Doc ())
                      -> (args : List $ Doc ())
                      -> Doc ()
typeDeclWithImplicits n = functionTypeWithImplicits n ':'

export
typeDecl : (name : IdrisIdent) -> (res : Doc ()) -> (args : List $ Doc ()) -> Doc ()
typeDecl n = functionType n ':'

--------------------------------------------------------------------------------
--          Function Application
--------------------------------------------------------------------------------

export
prettyParens : (b : Bool) -> Doc ann -> Doc ann
prettyParens True  = parens
prettyParens False = id

export
prettyCon : Prec -> (con : Doc ann) -> (args : List (Doc ann)) -> Doc ann
prettyCon p con args = prettyParens (p >= App) (con <++> align (sep args))

export
prettySingleCon : Pretty arg => Prec -> (con : Doc ann) -> arg -> Doc ann
prettySingleCon p con arg = prettyCon p con [prettyPrec App arg]

export
io : Pretty arg => Prec -> arg -> Doc ann
io p = prettySingleCon p "IO"

export
jsio : Pretty arg => Prec -> arg -> Doc ann
jsio p = prettySingleCon p "JSIO"

export
primIO : Pretty arg => Prec -> arg -> Doc ann
primIO p = prettySingleCon p "PrimIO"

export
prettyArg : (name : IdrisIdent) -> Doc ann -> Doc ann
prettyArg name tpe = parens $ hsep [pretty name,":",tpe]

--------------------------------------------------------------------------------
--          Foreign Function Implementations
--------------------------------------------------------------------------------

export
argNames : Stream String
argNames = "a" :: "b" :: "c" :: "d" :: "e" :: "f" :: "g" ::
           "h" :: "i" :: "j" :: "k" :: "l" :: "m" :: "n" :: 
           "o" :: "p" :: "q" :: "r" :: "s" :: "t" :: "u" :: 
           "v" :: "w" :: "y" :: "z" :: 
           map (\v => "x" ++ show v) [the Integer 1 ..]

export
unShadowingArgNames : IdrisIdent -> Stream String
unShadowingArgNames i = go (show i) argNames
  where go : String -> Stream String -> Stream String
        go s (h :: t) = if s == h then t else h :: go s t


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

foreignBrowser : String -> String
foreignBrowser s = "%foreign \"browser:lambda:" ++ s ++ "\""

export
attrGetFFI : AttributeName -> String
attrGetFFI n = foreignBrowser #"x=>x.\#{n.value}"#

export
staticAttrGetFFI : Kind -> AttributeName -> String
staticAttrGetFFI o n =
  foreignBrowser #"()=>\#{kindToString o}.\#{n.value}"#

export
attrSetFFI : AttributeName -> String
attrSetFFI n = foreignBrowser #"(x,v)=>{x.\#{n.value} = v}"#

export
staticAttrSetFFI : Kind -> AttributeName -> String
staticAttrSetFFI o n =
  foreignBrowser #"v=>{\#{kindToString o}.\#{n.value} = v}"#

export
funFFI : OperationName -> Nat -> String
funFFI n Z = foreignBrowser #"x=>x.\#{n.value}()"#
funFFI n k =
  let vs = take k argNames
      vals = fastConcat $ intersperse "," vs
      args = fastConcat $ intersperse " " vs
   in foreignBrowser #"(x,\#{vals})=>x.\#{n.value}(\#{args})"#

export
staticFunFFI : Kind -> OperationName -> Nat -> String
staticFunFFI o n Z = foreignBrowser #"x=>x.\#{n.value}()"#
staticFunFFI o n k =
  let vs = take k argNames
      vals = fastConcat $ intersperse "," vs
      args = fastConcat $ intersperse " " vs
   in foreignBrowser #"(\#{vals})=>\#{kindToString o}.\#{n.value}(\#{args})"#

export
conFFI : Kind -> Nat -> String
conFFI n k =
  let vs = take k argNames
      vals = fastConcat $ intersperse "," vs
      args = fastConcat $ intersperse " " vs
   in foreignBrowser #"(\#{vals})=> new \#{kindToString n}(\#{args})"#

export
dictConFFI : List ArgumentName -> String
dictConFFI ns =
  let vs     = take (length ns) argNames
      vals   = fastConcat $ intersperse "," vs
      fields = fastConcat $ intersperse "," (zipWith app vs ns)
   in foreignBrowser #"(\#{vals})=> {\#{fields}}"#

  where app : String -> ArgumentName -> String
        app v a = a.value ++ ": " ++ v

export
getterFFI : String
getterFFI = foreignBrowser "(o,x)=>o[x]"

export
setterFFI : String
setterFFI = foreignBrowser "(o,x,v)=>o[x] = v"
