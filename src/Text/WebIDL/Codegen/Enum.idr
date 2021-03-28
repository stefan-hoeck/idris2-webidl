module Text.WebIDL.Codegen.Enum

import Data.List
import Text.WebIDL.Codegen.Util

enum : Codegen Enum
enum (MkEnum _ name vs) =
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
                   , typeDecl "read" ("Maybe" <++> pn) [ pretty "String" ]

                   , vsep $ zipWith readImpl (s :: ss) (c :: cs)
                   , "read _ = Nothing"
                   , ""
                   , "public export"
                   , typeDecl "fromString" pn
                       [ "(s : String)"
                       , "{auto 0 _ : IsJust (" <+> pn  <+> ".read s)}"
                       ]
                   , "fromString s = fromJust $ read s"
                   , ""
                   , "export"
                   , "ToJS" <++> pn <++> "where"
                   , indent 2 ("toJS = toJS . show")
                   , ""
                   , "export"
                   , "FromJS" <++> pn <++> "where"
                   , "  fromJS ptr = fromJS ptr >>= read"
                   ]

   in vsep ["", "namespace" <++> pn, indent 2 code]

  where showImpl : String -> String -> Doc ()
        showImpl x y = hsep ["show",pretty x,"=",pretty y]

        readImpl : String -> String -> Doc ()
        readImpl x y = hsep ["read",pretty x,"=","Just", pretty y]

export
enums : List Enum -> String
enums = section "Enums" . map (show . enum)
