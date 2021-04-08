module Text.WebIDL.Codegen.Definitions

import Data.List
import Data.List.Elem
import Data.SOP
import Data.String
import Text.WebIDL.Codegen.Enum
import Text.WebIDL.Codegen.Members
import Text.WebIDL.Codegen.Rules
import Text.WebIDL.Codegen.Types
import public Text.WebIDL.Codegen.Util

--------------------------------------------------------------------------------
--          Imports
--------------------------------------------------------------------------------

defImports : CGDomain -> String
defImports d = #"""
               import JS
               import Web.Internal.\#{d.name}Prim
               import Web.Internal.Types
               """#

typeImports : String
typeImports = "import JS"

--------------------------------------------------------------------------------
--          Data Declarations
--------------------------------------------------------------------------------

extern : CGDomain -> String
extern d = fastUnlines [ section "Interfaces" $ exts ext name d.ifaces
                       , section "Dictionaries" $ exts ext name d.dicts
                       , section "Mixins" $ exts extNoCast name d.mixins
                       , section "Callbacks" $ exts extNoCast name d.callbacks
                       ]
  where ext : String -> String
        ext s = #"""
                export data \#{s} : Type where [external]
                
                export
                SafeCast \#{s} where
                  safeCast = unsafeCastOnPrototypeName "\#{s}"
                """#

        extNoCast : String -> String
        extNoCast s = #"export data \#{s} : Type where [external]"#

        exts :  (f : String -> String)
             -> (a -> Identifier)
             -> List a
             -> List String
        exts f g = map (("\n" ++) . f) . sort . map (value . g)

--------------------------------------------------------------------------------
--          CallbackInterfaces
--------------------------------------------------------------------------------

callback : CGCallback -> String
callback (MkCallback n cs _ _) =
   namespaced n $ constants cs

callbacks : CGDomain -> String
callbacks = section "Callbacks"
          . map callback
          . sortBy (comparing name) 
          . callbacks

--------------------------------------------------------------------------------
--          Interfaces
--------------------------------------------------------------------------------

ifaces' : (CGIface -> List String) -> CGDomain -> String
ifaces' f = section "Interfaces" . map ns . sortBy (comparing name) . ifaces
  where ns : CGIface -> String
        ns i = namespaced i.name (f i)

ifaces : CGDomain -> String
ifaces = ifaces' \(MkIface n s cs fs) =>
         jsType n s :: constants cs ++ functions fs

primIfaces : CGDomain -> String
primIfaces = ifaces' (primFunctions . functions)

--------------------------------------------------------------------------------
--          Dictionaries
--------------------------------------------------------------------------------

dicts' : (CGDict -> List String) -> CGDomain -> String
dicts' f = section "Dictionaries" . map ns . sortBy (comparing name) . dicts
  where ns : CGDict -> String
        ns d = namespaced d.name (f d)

dicts : CGDomain -> String
dicts = dicts' \(MkDict n s fs) => jsType n s :: functions fs

primDicts : CGDomain -> String
primDicts = dicts' (primFunctions . functions)

--------------------------------------------------------------------------------
--          Mixins
--------------------------------------------------------------------------------

mixins' : (CGMixin -> List String) -> CGDomain -> String
mixins' f = section "Mixins" . map ns . sortBy (comparing name) . mixins
  where ns : CGMixin -> String
        ns m = namespaced m.name (f m)

mixins : CGDomain -> String
mixins = mixins' \(MkMixin n cs fs) => constants cs ++ functions fs

primMixins : CGDomain -> String
primMixins = mixins' (primFunctions . functions)

--------------------------------------------------------------------------------
--          Typedefs
--------------------------------------------------------------------------------

export
typedefs : List Domain -> String
typedefs ds =
  let ts   = concatMap typedefs ds
      docs =  map toTypedef (sortBy (comparing name) ts)

      sect = section "Typedefs" $
               ["", "mutual"] ++ map (show . indent 2) docs
   in #"""
      module Web.Internal.Types
      
      import JS
      import public Web.Internal.AnimationTypes as Types
      import public Web.Internal.ClipboardTypes as Types
      import public Web.Internal.CssTypes as Types
      import public Web.Internal.DomTypes as Types
      import public Web.Internal.FetchTypes as Types
      import public Web.Internal.FileTypes as Types
      import public Web.Internal.GeometryTypes as Types
      import public Web.Internal.HtmlTypes as Types
      import public Web.Internal.MediasourceTypes as Types
      import public Web.Internal.MediastreamTypes as Types
      import public Web.Internal.PermissionsTypes as Types
      import public Web.Internal.ServiceworkerTypes as Types
      import public Web.Internal.StreamsTypes as Types
      import public Web.Internal.SvgTypes as Types
      import public Web.Internal.UIEventsTypes as Types
      import public Web.Internal.UrlTypes as Types
      import public Web.Internal.VisibilityTypes as Types
      import public Web.Internal.WebglTypes as Types
      import public Web.Internal.WebidlTypes as Types
      import public Web.Internal.XhrTypes as Types
      \#{sect}
      """#
      
  where toTypedef : Typedef -> Doc ()
        toTypedef t = vsep [ ""
                           , "public export"
                           , "0" <++> pretty t.name.value <++> ": Type"
                           , pretty t.name.value <++> "=" <++> pretty t.type
                           ]

--------------------------------------------------------------------------------
--          Codegen
--------------------------------------------------------------------------------
-- 
export
types : CGDomain -> String
types d =
  #"""
  module Web.Internal.\#{d.name}Types
   
  \#{typeImports}
  \#{enums d.enums}
  \#{extern d}
  """#

export
primitives : CGDomain -> String
primitives d =
  #"""
  module Web.Internal.\#{d.name}Prim
   
  import JS
  import Web.Internal.Types
   
  \#{primIfaces d}
  \#{primMixins d}
  \#{primDicts d}
  """#

export
definitions : CGDomain -> String
definitions d =
  #"""
  module Web.\#{d.name}
   
  \#{defImports d}
  \#{ifaces d}
  \#{mixins d}
  \#{dicts d}
  \#{callbacks d}
  """#
