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
functionType :  (name : String)
             -> (sep : Char)
             -> (res : Doc ())
             -> (args : List $ Doc ())
             -> Doc ()
functionType n c res []        = hsep [pretty n, pretty c, res]
functionType n c res (h :: t)  =
  let h' = pretty c <++> flatAlt (" "  <+> h) h
   in pretty n <++> align (sep (h' :: map ("->" <++>) (t ++ [res])))

export
typeDecl : (name : String) -> (res : Doc ()) -> (args : List $ Doc ()) -> Doc ()
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

renameArg : String -> String
renameArg "covering"  = "covering_"
renameArg "data"      = "data_"
renameArg "export"    = "export_"
renameArg "interface" = "interface_"
renameArg "module"    = "module_"
renameArg "private"   = "private_"
renameArg "prefix"    = "prefix_"
renameArg "public"    = "public_"
renameArg "record"    = "record_"
renameArg "total"     = "total_"
renameArg x           = x

export
prettyArg : (name : String) -> Doc ann -> Doc ann
prettyArg name tpe = parens $ hsep [pretty $ renameArg name,":",tpe]
