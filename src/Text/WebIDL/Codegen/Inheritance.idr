module Text.WebIDL.Codegen.Inheritance

import Data.SortedMap
import Text.WebIDL.Types

%default total

public export
record JSType where
  constructor MkJSType
  parent : Maybe Identifier
  mixins : List Identifier

public export
JSTypes : Type
JSTypes = SortedMap Identifier JSType

export
jsTypes : List Domain -> JSTypes
jsTypes ds =
  let types =  (ds >>= map dictToType . dictionaries)
            ++ (ds >>= map interfaceToType . interfaces)

      includes = ds >>= includeStatements

      initialMap = SortedMap.fromList types

   in foldl mixin initialMap includes

  where dictToType : Dictionary -> (Identifier,JSType)
        dictToType (MkDictionary _ n i _) = (n, MkJSType i Nil)

        interfaceToType : Interface -> (Identifier,JSType)
        interfaceToType (MkInterface _ n i _) = (n, MkJSType i Nil)

        mixin : JSTypes -> Includes -> JSTypes
        mixin ts (MkIncludes _ n incl) =
          case lookup n ts of
               Nothing => ts
               Just js => let js2 = record {mixins $= (incl ::)} js
                           in insert n js2 ts

public export
record Supertypes where
  constructor MkSupertypes
  parents : List Identifier
  mixins  : List Identifier

objectOnly : Supertypes
objectOnly = MkSupertypes [MkIdent "JSObject"] []

||| Calculates the supertypes and mixins for a given
||| identifier.
|||
|||  @maxIterations : Maximal number of iterations. Without this,
|||                   the algorithm might loop forever in case of
|||                   cyclic dependencies.
export
supertypes : JSTypes -> (maxIterations : Nat) -> Identifier -> Supertypes
supertypes _   0    i = objectOnly
supertypes js (S k) i =
  case lookup i js of
       Nothing                              => objectOnly

       Just $ MkJSType Nothing mixins       =>
         record { mixins = mixins } objectOnly

       Just $ MkJSType (Just parent) mixins =>
         let MkSupertypes parents mixins2 = supertypes js k parent
          in MkSupertypes (parent :: parents) (mixins ++ mixins2)
