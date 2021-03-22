module Text.WebIDL.Codegen.Definitions

import Data.String
import Data.SortedMap
import Data.SortedSet
import Text.WebIDL.Codegen.Enum
import public Text.WebIDL.Codegen.Util

--------------------------------------------------------------------------------
--          Imports
--------------------------------------------------------------------------------

imports : Definitions -> SortedSet String
imports ds = fromList $ enumImports

  where enumImports : List String
        enumImports = guard (not $ null ds.enums) *> ["Data.Maybe"]

--------------------------------------------------------------------------------
--          Codegen
--------------------------------------------------------------------------------

export
definitions : (moduleName : String) -> Codegen Definitions
definitions moduleName ds =
  let imps = vsep . map (("import" <++>) . pretty) . toList $ imports ds
   in vsep [ "module" <++> pretty moduleName
           , ""
           , imps
           , enums $ map snd ds.enums
           ]
