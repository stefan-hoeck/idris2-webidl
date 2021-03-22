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
function : (name : String) -> (args : Vect (S n) $ Doc ()) -> Doc ()
function n (h :: []) = hsep [pretty n, ":", h]
function n (h :: t)  =
  let h' = ":" <++> flatAlt (" "  <+> h) h
   in pretty n <++> align (sep (h' :: map ("->" <++>) (toList t)))
