module Text.WebIDL.Codegen.Util

import Data.List
import public Data.String
import public Data.Vect
import public Text.PrettyPrint.Prettyprinter
import public Text.WebIDL.Types

public export
0 Codegen : Type -> Type
Codegen a = a -> Doc ()

--------------------------------------------------------------------------------
--          Sorted Lists
--------------------------------------------------------------------------------

export
sortedNubOn : Ord b => (a -> b) -> List a -> List a
sortedNubOn f = nub . sortBy (comparing f)
  where nub : List a -> List a
        nub (x :: t@(y :: ys)) = if f x == f y then nub t else x :: nub t
        nub xs                 = xs

--------------------------------------------------------------------------------
--          Modules
--------------------------------------------------------------------------------

export
moduleName : String -> String
moduleName s = case fastUnpack s of
                    []       => ""
                    (h :: t) => fastPack (toUpper h :: t)

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
title : String -> Doc ()
title n = let ln = pretty $ fastPack (replicate 80 '-')
           in vsep ["", ln, pretty ("--          " ++ n), ln]

export
section : String -> List (Doc ()) -> Doc ()
section _ Nil = neutral
section t ds = vsep $ (title t) :: ds

--------------------------------------------------------------------------------
--          Namespaces Implementations
--------------------------------------------------------------------------------

export
namespaced : Identifier -> List $ Doc () -> List $ Doc ()
namespaced _ [] = neutral
namespaced n ds = "" :: ("namespace" <++> pretty n.value) :: map (indent 2) ds

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
primIO : Pretty arg => Prec -> arg -> Doc ann
primIO p = prettySingleCon p "PrimIO"

export
prettyArg : (name : IdrisIdent) -> Doc ann -> Doc ann
prettyArg name tpe = parens $ hsep [pretty name,":",tpe]

--------------------------------------------------------------------------------
--          Foreign Function Implementations
--------------------------------------------------------------------------------

export
mapFirstChar : (Char -> Char) -> String -> String
mapFirstChar f x = case fastUnpack x of
                        []       => ""
                        (h :: t) => fastPack (f h :: t)

foreignBrowser : String
foreignBrowser ="%foreign \"browser:lambda:"

export
attrGet : String -> Doc ann
attrGet n = pretty $ foreignBrowser ++ "x=>x." ++ n ++ "\""

export
attrSet : String -> Doc ann
attrSet n = pretty $ foreignBrowser ++ "(x,v)=>{x." ++ n ++ " = v}\""

export
setter : String -> IdrisIdent
setter = fromString . ("set" ++) . mapFirstChar toUpper
