module Text.WebIDL.Codegen.Defs

import Data.List.Elem
import Data.SOP
import Data.SortedMap
import Text.WebIDL.Types

public export
0 NameMap : Type -> Type
NameMap = SortedMap String

mergeDict : PDictionary -> Dictionary -> Dictionary
mergeDict (MkPDictionary _ _ ms) = record { members $= (++ ms) }

mergeIface : PInterface -> Interface -> Interface
mergeIface (MkPInterface _ _ ms) = record { members $= (++ map to ms) }
  where to : (a,b) -> (a, NS I [c,b])
        to (x, y) = (x, inject y)

mergeMixin : PMixin -> Mixin -> Mixin
mergeMixin (MkPMixin _ _ ms) = record { members $= (++ ms) }

mergeNamespace : PNamespace -> Namespace -> Namespace
mergeNamespace (MkPNamespace _ _ ms) = record { members $= (++ ms) }

public export
record Defs where
  constructor MkDefs
  domain              : String
  callbackInterfaces  : NameMap CallbackInterface
  callbacks           : NameMap Callback
  dictionaries        : NameMap Dictionary
  enums               : NameMap Enum
  includeStatements   : NameMap Includes
  interfaces          : NameMap Interface
  mixins              : NameMap Mixin
  namespaces          : NameMap Namespace
