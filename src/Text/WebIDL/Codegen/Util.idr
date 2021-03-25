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
--          Generating Functions
--------------------------------------------------------------------------------

export
function : (name : String) -> (res : Doc ()) -> (args : List $ Doc ()) -> Doc ()
function n res []        = hsep [pretty n, ":", res]
function n res (h :: t)  =
  let h' = ":" <++> flatAlt (" "  <+> h) h
   in pretty n <++> align (sep (h' :: map ("->" <++>) (t ++ [res])))

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
prettyArg : (name : String) -> Doc ann -> Doc ann
prettyArg name tpe = parens $ vsep [pretty name,":",tpe]
