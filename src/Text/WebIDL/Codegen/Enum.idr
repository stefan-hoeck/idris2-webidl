module Text.WebIDL.Codegen.Enum

import Data.List
import Text.WebIDL.Codegen.Util

export
data Foo : Type where [external]

enum : Codegen Enum
enum (MkEnum name vs) =
  let (s ::: ss) = map value vs
      (c ::: cs) = map toDataConstructor (s ::: ss)
      pn         = pretty name.value

      vals = ("=" <++> pretty c) :: map (\sl => ("|" <++> pretty sl)) cs

      code =  vsep [ ""
                   , "public export"
                   , "data" <++> pn <++> align (sep vals)
                   , ""
                   , "public export"
                   , "Show" <++> pn <++> "where"
                   , indent 2 $ vsep $ zipWith showImpl (c :: cs) (s :: ss)
                   , ""
                   , "public export"
                   , "Eq" <++> pn <++> "where"
                   , indent 2 $ "(==) = (==) `on` show"
                   , ""
                   , "public export"
                   , "Ord" <++> pn <++> "where"
                   , indent 2 $ "compare = compare `on` show"
                   , ""
                   , "public export"
                   , function "read" [ pretty "String"
                                     , "Maybe" <++> pretty name.value]
                   , vsep $ zipWith readImpl (s :: ss) (c :: cs)
                   , "read _ = Nothing"
                   , ""
                   , "public export"
                   , function "fromString"
                       [ "(s : String)"
                       , "{auto 0 _ : IsJust (" <+> pn  <+> ".read s)}"
                       , pn
                       ]
                   , "fromString s = fromJust $ read s"
                   ]

   in vsep ["", "namespace" <++> pn, indent 2 code]

  where showImpl : String -> String -> Doc ()
        showImpl x y = hsep ["show",pretty x,"=",pretty y]

        readImpl : String -> String -> Doc ()
        readImpl x y = hsep ["read",pretty x,"=","Just", pretty y]

export
enums : Codegen (List Enum)
enums es = section "Enums" (map enum es)
