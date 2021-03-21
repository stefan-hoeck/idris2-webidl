module Text.WebIDL.Codegen

import Data.List
import Data.List1
import Data.String
import Data.Vect
import Text.PrettyPrint.Prettyprinter
import Text.WebIDL.Types

public export
0 Codegen : Type -> Type
Codegen a = a -> Doc ()

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

function : (name : String) -> (args : Vect (S n) $ Doc ()) -> Doc ()
function n (h :: []) = hsep [pretty n, ":", h]
function n (h :: t)  =
  let h' = ":" <++> flatAlt (" "  <+> h) h
   in pretty n <++> align (sep (h' :: map ("->" <++>) (toList t)))

--------------------------------------------------------------------------------
--          Enums
--------------------------------------------------------------------------------

unquote : String -> List Char
unquote = run . fastUnpack
  where run : List Char -> List Char
        run []                   = []
        run ('\\' :: '"' :: cs)  = '"' :: run cs
        run ('"' :: cs)          = run cs
        run (c   :: cs)          = c :: run cs

toDataConstructor : String -> String
toDataConstructor s =
  case unquote s of
       []      => "Empty"
       c :: cs => fastPack (toUpper c :: run cs)

  where run : List Char -> List Char
        run []             = []
        run (x :: c :: cs) = 
          if isAlphaNum x then x :: run (c :: cs) else toUpper c :: run cs
        run (c :: cs)      = c :: run cs


export
enum : Codegen Enum
enum (MkEnum name vs) =
  let (s ::: ss) = map value vs
      (c ::: cs) = map toDataConstructor (s ::: ss)

      vals = ("=" <++> pretty c) :: map (\sl => ("|" <++> pretty sl)) cs

      code =  vsep [ "public export"
                   , "data" <++> pretty name.value <++> align (sep vals)
                   , ""
                   , "public export"
                   , "Show" <++> pretty name.value <++> "where"
                   , indent 2 $ vsep $ zipWith showImpl (c :: cs) (s :: ss)
                   , ""
                   , "public export"
                   , function "read" [ pretty "String"
                                     , "Maybe" <++> pretty name.value]
                   , vsep $ zipWith readImpl (s :: ss) (c :: cs)
                   , "read _ = Nothing"
                   , ""
                   , "public export"
                   , function "fromString" [ "(s : String)"
                                           , "{auto 0 _ : IsJust (read s)}"
                                           , pretty name.value
                                           ]
                   , "fromString s = fromJust $ read s"
                   ]

   in vsep ["namespace" <++> pretty name.value, "", indent 2 code, ""]

  where showImpl : String -> String -> Doc ()
        showImpl x y = hsep ["show",pretty x,"=",pretty y]

        readImpl : String -> String -> Doc ()
        readImpl x y = hsep ["read",pretty x,"=","Just", pretty y]
