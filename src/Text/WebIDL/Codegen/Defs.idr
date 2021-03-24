module Text.WebIDL.Codegen.Defs

import Data.List.Elem
import Data.SOP
import Text.WebIDL.Types

updateWith : Eq k =>
             (a -> b -> b) -> (a -> k) -> (b -> k) -> a -> List b -> List b
updateWith _ _  _  _ []        = []
updateWith f ak bk a (b :: bs) = let b2 = if ak a == bk b then f a b else b
                                  in b2 :: updateWith f ak bk a bs

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
  callbacks           : List Callback
  callbackInterfaces  : List CallbackInterface
  dictionaries        : List Dictionary
  enums               : List Enum
  includeStatements   : List Includes
  interfaces          : List Interface
  mixins              : List Mixin
  namespaces          : List Namespace
  typedefs            : List Typedef

toDefs : List (String,PartsAndDefs) -> List Defs
toDefs ps = 
  let defs = map (\(s,pad) => fromNP s (defs pad)) ps
      prts = concatMap (parts . snd) ps
   in map (applyParts prts) defs

  where fromNP : String -> Definitions -> Defs
        fromNP s [c,ci,d,e,ic,it,m,n,t] = MkDefs s c ci d e ic it m n t

        applyParts : Parts -> Defs -> Defs
        applyParts [d,i,m,n] = ?foo
