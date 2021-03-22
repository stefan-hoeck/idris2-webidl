module Text.WebIDL.Codegen.Enum

import Data.List
import Text.WebIDL.Codegen.Util


enum : Codegen Enum
enum (MkEnum name vs) =
  let (s ::: ss) = map value vs
      (c ::: cs) = map toDataConstructor (s ::: ss)

      vals = ("=" <++> pretty c) :: map (\sl => ("|" <++> pretty sl)) cs

      code =  vsep [ ""
                   , "public export"
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

export
enums : Codegen (List Enum)
enums [] = neutral
enums es = vsep (title "Enums" :: map enum es)
