module Text.WebIDL.Codegen.Definitions

import Data.List
import Data.String
import Data.SortedMap
import Data.SortedSet
import Text.WebIDL.Codegen.Enum
import public Text.WebIDL.Codegen.Util

--------------------------------------------------------------------------------
--          Imports
--------------------------------------------------------------------------------

defImports : (moduleName : String) -> Definitions -> SortedSet String
defImports mn ds = fromList ["JS.DOM.Raw.Types"]

typeImports : Definitions -> SortedSet String
typeImports ds = fromList enumImports

  where enumImports : List String
        enumImports = guard (not $ null ds.enums) *> ["Data.Maybe"]

--------------------------------------------------------------------------------
--          Data Declarations
--------------------------------------------------------------------------------

extern : Codegen Definitions 
extern ds = vsep [ section "Interfaces" (exts name ds.interfaces)
                 , section "Mixins" (exts name ds.mixins)
                 , section "Dictionaries" (exts name ds.dictionaries)
                 ]
  where ext : String -> Doc ()
        ext s = vsep [ ""
                     , "export"
                     ,"data" <++> pretty s <++> ": Type where [external]"
                     ]

        exts : (a -> Identifier) -> List (x,a) -> List (Doc ())
        exts f = map ext . sort . map (value . f . snd)

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
                    -> List (x,a)
                    -> List (String,String)
        inheritance i n = mapMaybe \(_,v) =>
                            map (\to => (value (n v), value to)) (i v)

        pairs : List (String,String)
        pairs =  inheritance inherits name ds.interfaces
              ++ inheritance inherits name ds.dictionaries
              ++ map (\(_,s) => (s.name.value,s.includes.value))
                     ds.includeStatements

--------------------------------------------------------------------------------
--          Codegen
--------------------------------------------------------------------------------

export
types : (moduleName : String) -> Codegen Definitions
types moduleName ds =
  let imps = vsep 
           . map (("import" <++>) . pretty) 
           . SortedSet.toList $ typeImports ds

   in vsep [ "module JS.DOM.Raw." <+> pretty moduleName <+> "Types"
           , ""
           , imps
           , enums $ map snd ds.enums
           , extern ds
           ]

export
definitions : (moduleName : String) -> Codegen Definitions
definitions moduleName ds =
  let imps = vsep 
           . map (("import" <++>) . pretty) 
           . SortedSet.toList $ defImports moduleName ds

   in vsep [ "module JS.DOM.Raw." <+> pretty moduleName
           , ""
           , imps
           , casts ds
           ]
