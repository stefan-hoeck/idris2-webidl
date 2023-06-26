module Text.WebIDL.Codegen.Definitions

import Data.List
import Data.List.Elem
import Data.SOP
import Data.String
import Text.WebIDL.Codegen.Args
import Text.WebIDL.Codegen.Enum
import Text.WebIDL.Codegen.Members
import Text.WebIDL.Codegen.Rules
import Text.WebIDL.Codegen.Types
import public Text.WebIDL.Codegen.Util

%default total

--------------------------------------------------------------------------------
--          Imports
--------------------------------------------------------------------------------

export
imports : SortedSet Identifier -> String
imports is =
  let ds := toList {t = SortedSet} is
   in fastUnlines $ "import JS" :: map (("import Web.Types." ++) . value) ds

--------------------------------------------------------------------------------
--          Type Declarations
--------------------------------------------------------------------------------

extNoCast : Identifier -> String
extNoCast s = """
  export data \{s} : Type where [external]

  export
  ToFFI \{s} \{s} where toFFI = id

  export
  FromFFI \{s} \{s} where fromFFI = Just
  """

extWithCast : Identifier -> String
extWithCast s = extNoCast s ++ "\n\n" ++ """
  export
  SafeCast \{s} where
    safeCast = unsafeCastOnPrototypeName "\{s}"
  """

ext : (withCast : Bool) -> Identifier -> String
ext False = extNoCast
ext True  = extWithCast

typeFile : Identifier -> Maybe Supertypes -> (withCast : Bool) -> String
typeFile n super b =
  """
  module Web.Types.\{n}

  \{imports (delete n $ depends super)}

  %default total

  \{ext b n}

  \{maybe neutral (fastUnlines . casts n) super}
  """

export
dictType : CGDict -> String
dictType d = typeFile d.name (Just d.super) False

export
ifaceType : CGIface -> String
ifaceType d = typeFile d.name (Just d.super) True

export
mixinType : CGMixin -> String
mixinType d = typeFile d.name Nothing False

export
callbackType : CGCallback -> String
callbackType d = typeFile d.name Nothing False


--------------------------------------------------------------------------------
--          Prim Declarations
--------------------------------------------------------------------------------

funFile : Identifier -> SortedSet Identifier -> List String -> String
funFile n ds ss =
  """
  module Web.Raw.\{n}

  \{imports ds}

  %default total

  \{fastUnlines ss}
  """

export
dict : CGDict -> String
dict d@(MkDict n _ fs) =
  funFile n (depends d) (primFunctions fs ++ functions fs)

export
iface : CGIface -> String
iface d@(MkIface n _ cs fs) =
  funFile n (depends d) (constants cs ++ primFunctions fs ++ functions fs)

export
mixin : CGMixin -> String
mixin d@(MkMixin n cs fs) =
  funFile n (depends d) (constants cs ++ primFunctions fs ++ functions fs)

export
callback : CGCallback -> String
callback d@(MkCallback n cs _ _) =
  funFile n (depends d) (primCallback d :: Members.callback d :: constants cs)
