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

defImports : Domain -> String
defImports d = #"""
               import JS
               import Web.Internal.\#{d.domain}Prim
               import Web.Types
               """#

typeImports : String
typeImports = "import JS"

--------------------------------------------------------------------------------
--          Data Declarations
--------------------------------------------------------------------------------

extern : Domain -> String
extern d = fastUnlines [ section "Interfaces" $ exts ext name d.interfaces
                       , section "Mixins" $ exts extMixin name d.mixins
                       , section "Dictionaries" $ exts ext name d.dictionaries
                       ]
  where ext : String -> String
        ext s = #"""
                export data \#{s} : Type where [external]
                
                export
                SafeCast \#{s} where
                  safeCast = unsafeCastOnPrototypeName "\#{s}"
                
                export ToJS \#{s} where toJS = believe_me

                export FromJS \#{s} where fromJS = safeCast
                """#

        extMixin : String -> String
        extMixin s = #"""
                export data \#{s} : Type where [external]
                
                export ToJS \#{s} where toJS = believe_me

                export FromJS \#{s} where fromJS ptr = Just (believe_me ptr)
                """#

        exts :  (f : String -> String)
             -> (a -> Identifier)
             -> List a
             -> List String
        exts f g = map (("\n" ++) . f) . sort . map (value . g)

--------------------------------------------------------------------------------
--          CallbackInterfaces
--------------------------------------------------------------------------------

callbackInterface : CallbackInterface -> String
callbackInterface (MkCallbackInterface _ n ms) =
   namespaced n $ constants (mapMaybe const ms)

callbackInterfaces : Domain -> String
callbackInterfaces = section "Callback Interfaces"
                   . map callbackInterface 
                   . sortBy (comparing name) 
                   . callbackInterfaces

--------------------------------------------------------------------------------
--          Interfaces
--------------------------------------------------------------------------------

interfaces_ : (Interface -> String) -> Domain -> String
interfaces_ f =
  section "Interfaces" . map f . sortBy (comparing name) . interfaces

primInterfaces : Env -> Domain -> String
primInterfaces e = interfaces_ iface
  where iface : Interface -> String
        iface (MkInterface _ n _ ms) =
          namespaced n
            $  readOnlyAttributesPrim e n (mapMaybe (part attrRO) ms)
            ++ attributesPrim e n (mapMaybe (part attr) ms)

interfaces : Env -> Domain -> String
interfaces e = interfaces_ iface
  where iface : Interface -> String
        iface (MkInterface _ n _ ms) =
          namespaced n
            $  jsType e n
            :: constants (mapMaybe (part const) ms)
            ++ readOnlyAttributes e n (mapMaybe (part attrRO) ms)
            ++ attributes e n (mapMaybe (part attr) ms)

--------------------------------------------------------------------------------
--          Dictionaries
--------------------------------------------------------------------------------

dictionaries_ : (Dictionary -> String) -> Domain -> String
dictionaries_ f =
  section "Dictionaries" . map f . sortBy (comparing name) . dictionaries

primDictionaries : Env -> Domain -> String
primDictionaries e = dictionaries_ dictionary
  where dictionary : Dictionary -> String
        dictionary (MkDictionary _ n _ ms) =
          namespaced n
            $  attributesPrim e n (mapMaybe required ms)
            ++ attributesPrim e n (mapMaybe optional ms)

dictionaries : Env -> Domain -> String
dictionaries e = dictionaries_ dictionary
  where dictionary : Dictionary -> String
        dictionary (MkDictionary _ n _ ms) =
          namespaced n
            $  jsType e n
            :: attributes e n (mapMaybe required ms)
            ++ attributes e n (mapMaybe optional ms)

--------------------------------------------------------------------------------
--          Mixins
--------------------------------------------------------------------------------

mixins_ : (Mixin -> String) -> Domain -> String
mixins_ f = section "Mixins" . map f . sortBy (comparing name) . mixins

primMixins : Env -> Domain -> String
primMixins e = mixins_ mixin
  where mixin : Mixin -> String
        mixin (MkMixin _ n ms) =
           namespaced n
             $  readOnlyAttributes e n (mapMaybe attrRO ms)
             ++ attributes e n (mapMaybe attr ms)

mixins : Env -> Domain -> String
mixins e = mixins_ mixin
  where mixin : Mixin -> String
        mixin (MkMixin _ n ms) =
           namespaced n
             $  constants (mapMaybe const ms)
             ++ readOnlyAttributes e n (mapMaybe attrRO ms)
             ++ attributes e n (mapMaybe attr ms)

--------------------------------------------------------------------------------
--          Namespaces
--------------------------------------------------------------------------------

nspace : Namespace -> String
nspace (MkNamespace _ n ms) =
   namespaced n Nil

namespaces : Domain -> String
namespaces = section "Namespaces"
           . map nspace
           . sortBy (comparing name)
           . namespaces

--------------------------------------------------------------------------------
--          Callbacks
--------------------------------------------------------------------------------

callbacks : List Domain -> String
callbacks = fastUnlines
          . map (show . indent 2 . toCallback)
          . sortBy (comparing name)
          . concatMap allCallbacks

  where memberToCallback :  Identifier
                         -> CallbackInterfaceMember
                         -> Maybe Callback
        memberToCallback n (Z _) = Nothing
        memberToCallback n (S $ Z $ MkOp () t _ as) =
           Just $ MkCallback [] n t as

        interfaceCallbacks : CallbackInterface -> List Callback
        interfaceCallbacks (MkCallbackInterface _ n ms) =
          mapMaybe (memberToCallback n . snd) ms

        allCallbacks : Domain -> List Callback
        allCallbacks d = d.callbacks ++
                         (d.callbackInterfaces >>= interfaceCallbacks)

        toCallback : Callback -> Doc ()
        toCallback (MkCallback _ n t args) =
          let ii = the IdrisIdent (fromString n.value)
           in vsep [ ""
                   , "public export"
                   , "0" <++> pretty ii <++> ": Type"
                   , functionType ii '=' (callbackReturnType t) $
                       case args of
                            [] => ["()"]
                            _  => map (pretty . snd) args
                   ]

--------------------------------------------------------------------------------
--          Typedefs
--------------------------------------------------------------------------------

export
typedefs : List Domain -> String
typedefs ds =
  let ts   = concatMap typedefs ds
      docs =  map toTypedef (sortBy (comparing name) ts)

      sect = section "Typedefs and Callbacks" $
               ["", "mutual", callbacks ds] ++
               map (show . indent 2) docs
   in #"""
      module Web.Types
      
      import JS
      import public Web.AnimationTypes as Types
      import public Web.ClipboardTypes as Types
      import public Web.CssTypes as Types
      import public Web.DomTypes as Types
      import public Web.EventTypes as Types
      import public Web.FetchTypes as Types
      import public Web.FileTypes as Types
      import public Web.GeometryTypes as Types
      import public Web.HtmlTypes as Types
      import public Web.MediasourceTypes as Types
      import public Web.MediastreamTypes as Types
      import public Web.PermissionsTypes as Types
      import public Web.ServiceworkerTypes as Types
      import public Web.StreamsTypes as Types
      import public Web.SvgTypes as Types
      import public Web.UrlTypes as Types
      import public Web.VisibilityTypes as Types
      import public Web.WebglTypes as Types
      import public Web.WebidlTypes as Types
      import public Web.XhrTypes as Types
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

export
types : Domain -> String
types d =
  #"""
  module Web.\#{d.domain}Types

  \#{typeImports}
  \#{enums d.enums}
  \#{extern d}
  """#

export
primitives : Env -> Domain -> String
primitives e d =
  #"""
  module Web.Internal.\#{d.domain}Prim

  \#{primInterfaces e d}
  \#{primMixins e d}
  \#{primDictionaries e d}
  """#

export
definitions : Env -> Domain -> String
definitions e d =
  #"""
  module Web.\#{d.domain}

  \#{defImports d}
  \#{interfaces e d}
  \#{mixins e d}
  \#{dictionaries e d}
  \#{callbackInterfaces d}
  \#{namespaces d}
  """#
