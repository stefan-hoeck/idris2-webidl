module Text.WebIDL.Codegen.Definitions

import Data.List
import Data.List.Elem
import Data.SOP
import Data.String
import Text.WebIDL.Codegen.Enum
import Text.WebIDL.Codegen.Members
import Text.WebIDL.Codegen.Types
import public Text.WebIDL.Codegen.Util

--------------------------------------------------------------------------------
--          Imports
--------------------------------------------------------------------------------

defImports : Domain -> List String
defImports _ = ["JS.Util","Web.Types"]

typeImports : Domain -> List String
typeImports d =  "JS.Util" :: enumImports

  where enumImports : List String
        enumImports = guard (not $ null d.enums) *> ["Data.Maybe"]

--------------------------------------------------------------------------------
--          Data Declarations
--------------------------------------------------------------------------------

extern : Codegen Domain 
extern d = vsep [ section "Interfaces" $ exts name d.interfaces
                , section "Mixins" $ exts name d.mixins
                , section "Dictionaries" $ exts name d.dictionaries
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

casts : Codegen Domain
casts d = section "Casts" (map toCast $ sort pairs)
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
        pairs =  inheritance inherits name d.interfaces
              ++ inheritance inherits name d.dictionaries
              ++ map (\s => (s.name.value,s.includes.value))
                     d.includeStatements

--------------------------------------------------------------------------------
--          CallbackInterfaces
--------------------------------------------------------------------------------

callbackInterface : CallbackInterface -> List (Doc ())
callbackInterface (MkCallbackInterface _ n ms) =
   namespaced n $ constants (mapMaybe const ms)

callbackInterfaces : Codegen Domain
callbackInterfaces d =
  section "Callback Interfaces"
  (sortBy (comparing name) d.callbackInterfaces >>= callbackInterface)

--------------------------------------------------------------------------------
--          Interfaces
--------------------------------------------------------------------------------

iface : Interface -> List (Doc ())
iface (MkInterface _ n _ ms) =
   namespaced n
     $  constants (mapMaybe (part const) ms)
     ++ readOnlyAttributes (mapMaybe (part attrRO) ms)
     ++ attributes (mapMaybe (part attr) ms)

interfaces : Codegen Domain
interfaces d =
  section "Interfaces"
  (sortBy (comparing name) d.interfaces >>= iface)

--------------------------------------------------------------------------------
--          Dictionaries
--------------------------------------------------------------------------------

dictionary : Dictionary -> List (Doc ())
dictionary (MkDictionary _ n _ ms) =
  namespaced n
    $  attributes (mapMaybe required ms)
    ++ attributes (mapMaybe optional ms)

dictionaries : Codegen Domain
dictionaries d =
  section "Dictionaries"
  (sortBy (comparing name) d.dictionaries >>= dictionary)

--------------------------------------------------------------------------------
--          Mixins
--------------------------------------------------------------------------------

mixin : Mixin -> List (Doc ())
mixin (MkMixin _ n ms) =
   namespaced n
     $  constants (mapMaybe const ms)
     ++ readOnlyAttributes (mapMaybe attrRO ms)
     ++ attributes (mapMaybe attr ms)

mixins : Codegen Domain
mixins d =
  section "Mixins"
  (sortBy (comparing name) d.mixins >>= mixin)

--------------------------------------------------------------------------------
--          Namespaces
--------------------------------------------------------------------------------

nspace : Namespace -> List (Doc ())
nspace (MkNamespace _ n ms) =
   namespaced n Nil

namespaces : Codegen Domain
namespaces d =
  section "Namespaces"
  (sortBy (comparing name) d.namespaces >>= nspace)

--------------------------------------------------------------------------------
--          Callbacks
--------------------------------------------------------------------------------

callbacks : List Domain -> List (Doc ())
callbacks ds =
  let cs = concatMap allCallbacks ds
   in map toCallback $ sortBy (comparing name) cs

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
          vsep [ ""
               , "public export"
               , "0" <++> pretty n.value <++> ": Type"
               , functionType n.value '=' (returnType t) $
                   case args of
                        [] => ["()"]
                        _  => map (pretty . snd) args
               ]

--------------------------------------------------------------------------------
--          Typedefs
--------------------------------------------------------------------------------

export
typedefs : Codegen (List Domain)
typedefs ds =
  let ts   = concatMap typedefs ds
      docs =  map toTypedef (sortBy (comparing name) ts)
           ++ callbacks ds
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
           , "import public Web.MediasourceTypes as Types"
           , "import public Web.MediastreamTypes as Types"
           , "import public Web.PermissionsTypes as Types"
           , "import public Web.ServiceworkerTypes as Types"
           , "import public Web.StreamsTypes as Types"
           , "import public Web.SvgTypes as Types"
           , "import public Web.UrlTypes as Types"
           , "import public Web.VisibilityTypes as Types"
           , "import public Web.WebglTypes as Types"
           , "import public Web.WebidlTypes as Types"
           , "import public Web.XhrTypes as Types"
           , section "Typedefs and Callbacks" $
               ["", "mutual"] ++ map (indent 2) docs
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
typeTests : Codegen Domain
typeTests d =
  let ts = types d
      ps = zip [1 .. length ts] ts

   in vsep [ "module Test." <+> pretty d.domain <+> "Types"
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
types : Codegen Domain
types d =
  let imps = vsep 
           . map (("import" <++>) . pretty) 
           . sortedNubOn id $ typeImports d

   in vsep [ "module Web." <+> pretty d.domain <+> "Types"
           , ""
           , imps
           , enums d.enums
           , extern d
           ]

export
definitions : Codegen Domain
definitions d =
  let imps = vsep 
           . map (("import" <++>) . pretty) 
           . sortedNubOn id $ defImports d

   in vsep [ "module Web." <+> pretty d.domain
           , ""
           , imps
           , interfaces d
           , mixins d
           , dictionaries d
           , callbackInterfaces d
           , namespaces d
           , casts d
           ]
