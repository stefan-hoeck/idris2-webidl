module Text.WebIDL.Types.Definition

import Generics.Derive
import Text.WebIDL.Types.Attribute
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Type

%language ElabReflection


||| Typedef ::
|||     typedef TypeWithExtendedAttributes identifier ;
public export
data Definition : Type where
  Typedef :  (attributes : ExtAttributeList)
          -> (type       : IdlType)
          -> (name       : Identifier)
          -> Definition

%runElab derive "Definition" [Generic,Meta,Eq,Show]
