module Text.WebIDL.Codegen.Definitions

import Data.List
import Data.List.Elem
import Data.SOP
import Data.SortedMap
import Data.SortedSet
import Data.String
import Text.WebIDL.Codegen.Enum
import Text.WebIDL.Codegen.Types
import public Text.WebIDL.Codegen.Util

--------------------------------------------------------------------------------
--          Imports
--------------------------------------------------------------------------------

defImports : (moduleName : String) -> Definitions -> SortedSet String
defImports mn ds = fromList ["Web.Types"]

typeImports : Definitions -> SortedSet String
typeImports ds = fromList ( "JS.Util" :: enumImports)

  where enumImports : List String
        enumImports = guard (not $ null (get Enum ds)) *> ["Data.Maybe"]

--------------------------------------------------------------------------------
--          Data Declarations
--------------------------------------------------------------------------------

extern : Codegen Definitions 
extern ds = vsep [ section "Interfaces" . exts name $ get Interface ds
                 , section "Mixins" . exts name $ get Mixin ds
                 , section "Dictionaries" . exts name $ get Dictionary ds
                 ]
  where ext : String -> Doc ()
        ext s = vsep [ ""
                     , "export"
                     , "data" <++> pretty s <++> ": Type where [external]"
                     , ""
                     , "export"
                     , "ToJS" <++> pretty s <++> "where"
                     , indent 2 ("toJS = believe_me")
                     , ""
                     , "export"
                     , "FromJS" <++> pretty s <++> "where"
                     , indent 2 ("fromJS = believe_me")
                     ]

        exts : (a -> Identifier) -> List a -> List (Doc ())
        exts f = map ext . sort . map (value . f)

--------------------------------------------------------------------------------
--          Casts
--------------------------------------------------------------------------------

casts : Codegen Definitions
casts ds = section "Casts" (map toCast $ sort pairs)
  where toCast : (String,String) -> Doc ()
        toCast (from,to) =
          vsep [ ""
               , "export"
               , "Cast" <++> pretty from <++> pretty to <++> "where"
               , indent 2 ("cast = believe_me")
               ]

        inheritance :  (a -> Inheritance)
                    -> (a -> Identifier)
                    -> List a
                    -> List (String,String)
        inheritance i n = mapMaybe \v =>
                            map (\to => (value (n v), value to)) (i v)

        pairs : List (String,String)
        pairs =  inheritance inherits name (get Interface ds)
              ++ inheritance inherits name (get Dictionary ds)
              ++ map (\s => (s.name.value,s.includes.value))
                     (get Includes ds)

--------------------------------------------------------------------------------
--          Typedefs
--------------------------------------------------------------------------------

export
typedefs : Codegen Definitions
typedefs ds =
  let ts   = get Typedef ds
      docs = map toTypedef $ sortBy (comparing (value . name)) ts
   in vsep [ "module Web.Types"
           , ""
           , "import Data.SOP"
           , "import JS.Util"
           , "import public Web.AnimationTypes as Types"
           , "import public Web.ClipboardTypes as Types"
           , "import public Web.CssTypes as Types"
           , "import public Web.DomTypes as Types"
           , "import public Web.EventTypes as Types"
           , "import public Web.FetchTypes as Types"
           , "import public Web.FileTypes as Types"
           , "import public Web.GeometryTypes as Types"
           , "import public Web.HtmlTypes as Types"
           , "import public Web.PermissionsTypes as Types"
           , "import public Web.SvgTypes as Types"
           , "import public Web.UrlTypes as Types"
           , "import public Web.XhrTypes as Types"
           , section "Typedefs" $ ["", "mutual"] ++ map (indent 2) docs
           ]

  where toTypedef : Typedef -> Doc ()
        toTypedef t = vsep [ ""
                           , "public export"
                           , "0" <++> pretty t.name.value <++> ": Type"
                           , pretty t.name.value <++> "=" <++> pretty t.type
                           ]

--------------------------------------------------------------------------------
--          Codegen
--------------------------------------------------------------------------------

export
typeTests : (moduleName : String) -> Parts -> Definitions -> Doc ()
typeTests moduleName ds ps =
  let ts = types ds ++ types ps
      ps = zip [1 .. length ts] ts

   in vsep [ "module Test." <+> pretty moduleName <+> "Types"
           , ""
           , "import Data.SOP"
           , "import Web.Types"
           , "import JS.Util"
           , vsep (map mkTest ps)
           ]

  where mkTest : (Nat,IdlType) -> Doc ()
        mkTest (n,t) =
          let nm = pretty ("test" ++ show n)
           in vsep [ ""
                   , nm <++> ":" <++> pretty t <++> "-> ()"
                   , nm <++> "_ = ()"
                   ]

export
types : (moduleName : String) -> Codegen Definitions
types moduleName ds =
  let imps = vsep 
           . map (("import" <++>) . pretty) 
           . SortedSet.toList $ typeImports ds

   in vsep [ "module Web." <+> pretty moduleName <+> "Types"
           , ""
           , imps
           , enums $ get Enum ds
           , extern ds
           ]

export
definitions : (moduleName : String) -> Codegen Definitions
definitions moduleName ds =
  let imps = vsep 
           . map (("import" <++>) . pretty) 
           . SortedSet.toList $ defImports moduleName ds

   in vsep [ "module Web." <+> pretty moduleName
           , ""
           , imps
           , casts ds
           ]
