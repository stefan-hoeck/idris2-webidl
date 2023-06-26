module Text.WebIDL.Codegen.Enum

import Data.Refined
import Data.List
import Text.WebIDL.Codegen.Util

%default total

enum_ : {opts : _} -> Enum -> Doc opts
enum_ (MkEnum _ pn vs) =
  let (s ::: ss) := map value vs
      (c ::: cs) := map toDataConstructor (s ::: ss)
      vals       := the (List $ Doc opts) $ map (\sl => line "| \{sl}") cs
      sl         := line "data \{pn} =" <++> sep (line "\{c}" :: vals)
      ml         := vappend (line "data \{pn} =")
                      (indent 2 $ vsep (indent 2 (line "\{c}") :: vals))
   in vsep [ line "module Web.Types.\{pn}"
           , empty
           , "import JS"
           , empty
           , "%default total"
           , empty
           , line "public export"
           , ifMultiline sl ml
           , empty
           , line "export"
           , line "Show \{pn} where"
           , indent 2 $ vsep $ zipWith showImpl (c :: cs) (s :: ss)
           , empty
           , line "export"
           , line "Eq \{pn} where"
           , indent 2 $ line "(==) = (==) `on` show"
           , empty
           , line "export"
           , line "Ord \{pn} where"
           , indent 2 $ line "compare = compare `on` show"
           , empty
           , line "export"
           , typeDecl "read" (line "Maybe \{pn}") [ line "String" ]

           , vsep $ zipWith readImpl (s :: ss) (c :: cs)
           , line "read _ = Nothing"
           , empty
           , line "export"
           , line "ToFFI \{pn} String where"
           , indent 2 $ line "toFFI = show"
           , empty
           , line "export"
           , line "FromFFI \{pn} String where"
           , indent 2 $ line "fromFFI = read"
           ]

  where
    showImpl : String -> String -> Doc opts
    showImpl x y = line #"show \#{x} = "\#{y}""#

    readImpl : String -> String -> Doc opts
    readImpl x y = line #"read "\#{x}" = Just \#{y}"#

export %inline
enum : Enum -> String
enum = render80 . enum_
