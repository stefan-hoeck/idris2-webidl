module Text.WebIDL.Codegen.Enum

import Data.Refined
import Data.List
import Text.WebIDL.Codegen.Util

%default total

enum : {opts : _} -> Enum -> Doc opts
enum (MkEnum _ pn vs) =
  let (s ::: ss) := map value vs
      (c ::: cs) := map toDataConstructor (s ::: ss)
      vals       := the (List $ Doc opts) $ map (\sl => line "| \{sl}") cs
      sl         := line "data \{pn} =" <++> sep (line "\{c}" :: vals)
      ml         := vappend (line "data \{pn} =")
                      (indent 2 $ vsep (indent 2 (line "\{c}") :: vals))
      code =  vsep [ empty
                   , line "public export"
                   , ifMultiline sl ml
                   , empty
                   , line "public export"
                   , line "Show \{pn} where"
                   , indent 2 $ vsep $ zipWith showImpl (c :: cs) (s :: ss)
                   , empty
                   , line "public export"
                   , line "Eq \{pn} where"
                   , indent 2 $ line "(==) = (==) `on` show"
                   , empty
                   , line "public export"
                   , line "Ord \{pn} where"
                   , indent 2 $ line "compare = compare `on` show"
                   , empty
                   , line "public export"
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

   in vsep ["", line "namespace \{pn}", indent 2 code]

  where
    showImpl : String -> String -> Doc opts
    showImpl x y = line #"show \#{x} = "\#{y}""#

    readImpl : String -> String -> Doc opts
    readImpl x y = line #"read "\#{x}" = Just \#{y}"#

export
enums : List Enum -> String
enums = section "Enums" . map (render80 . enum)
