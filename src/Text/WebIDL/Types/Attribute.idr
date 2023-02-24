module Text.WebIDL.Types.Attribute

import Data.List1
import Derive.Prelude
import Language.Reflection.Util
import Data.SOP
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Symbol

%language ElabReflection

public export
isParenOrQuote : Char -> Bool
isParenOrQuote '(' = True
isParenOrQuote ')' = True
isParenOrQuote '[' = True
isParenOrQuote ']' = True
isParenOrQuote '{' = True
isParenOrQuote '}' = True
isParenOrQuote '"' = True
isParenOrQuote _   = False

public export
isCommaOrParenOrQuote : Char -> Bool
isCommaOrParenOrQuote ',' = True
isCommaOrParenOrQuote c   = isParenOrQuote c

public export
0 Other : Type
Other = NS I [IntLit,FloatLit,StringLit,Identifier,Keyword,Symbol]

||| ExtendedAttributeInner ::
public export
data EAInner : Type where
  |||   ( ExtendedAttributeInner ) ExtendedAttributeInner
  |||   [ ExtendedAttributeInner ] ExtendedAttributeInner
  |||   { ExtendedAttributeInner } ExtendedAttributeInner
  EAIParens : (inParens : EAInner) -> (eai : EAInner) -> EAInner

  |||   OtherOrComma ExtendedAttributeInner
  EAIOther  : (otherOrComma : Other) -> (eai : EAInner) -> EAInner

  |||   ε
  EAIEmpty  : EAInner

%runElab derive "EAInner" [Eq,Show]

namespace EAInner

  ||| Number of `Other`s.
  public export
  size : EAInner -> Nat
  size (EAIParens inParens eai)    = size inParens + size eai
  size (EAIOther otherOrComma eai) = 1 + size eai
  size EAIEmpty                    = 0

  ||| Number of `Other`s.
  public export
  leaves : EAInner -> Nat
  leaves (EAIParens inParens eai)    = leaves inParens + leaves eai
  leaves (EAIOther otherOrComma eai) = 1 + leaves eai
  leaves EAIEmpty                    = 1

  ||| Number of `Other`s.
  public export
  depth : EAInner -> Nat
  depth (EAIParens inParens eai)    = 1 + (depth inParens `max` depth eai)
  depth (EAIOther otherOrComma eai) = 1 + depth eai
  depth EAIEmpty                    = 0

||| ExtendedAttributeRest ::
|||   ExtendedAttribute
|||   ε
|||
||| ExtendedAttribute ::
public export
data ExtAttribute : Type where
  ||| ( ExtendedAttributeInner ) ExtendedAttributeRest
  ||| [ ExtendedAttributeInner ] ExtendedAttributeRest
  ||| { ExtendedAttributeInner } ExtendedAttributeRest
  EAParens : (inner : EAInner) -> (rest : Maybe ExtAttribute) -> ExtAttribute

  ||| Other ExtendedAttributeRest
  EAOther : (other : Other) -> (rest : Maybe ExtAttribute) -> ExtAttribute

%runElab derive "ExtAttribute" [Eq,Show]

namespace ExtAttribute

  ||| Number of `Other`s.
  public export
  size : ExtAttribute -> Nat
  size (EAParens inner rest) = size inner + maybe 0 size rest
  size (EAOther other rest)  = 1 + maybe 0 size rest

  ||| Number of leaves (unlike `size`, this includes empty leaves)
  public export
  leaves : ExtAttribute -> Nat
  leaves (EAParens inner rest) = leaves inner + maybe 1 leaves rest
  leaves (EAOther other rest)  = 1 + maybe 1 leaves rest

  ||| Number of `Other`s.
  public export
  depth : ExtAttribute -> Nat
  depth (EAParens inner rest) = 1 + (depth inner `max` maybe 0 depth rest)
  depth (EAOther other rest)  = 1 + maybe 0 depth rest


||| ExtendedAttributeList ::
|||   [ ExtendedAttribute ExtendedAttributes ]
|||   ε
|||
||| ExtendedAttributes ::
|||   , ExtendedAttribute ExtendedAttributes
|||   ε
public export
ExtAttributeList : Type
ExtAttributeList = List ExtAttribute

||| TypeWithExtendedAttributes ::
|||     ExtendedAttributeList Type
public export
Attributed : Type -> Type
Attributed a = (ExtAttributeList, a)

public export
interface HasAttributes a where
  constructor MkHasAttributes
  attributes : a -> ExtAttributeList

public export
HasAttributes () where
  attributes = const Nil

public export
HasAttributes String where
  attributes = const Nil

public export %inline
HasAttributes Identifier where
  attributes = const Nil

public export %inline
HasAttributes Bool where
  attributes = const Nil

public export %inline
HasAttributes FloatLit where
  attributes = const Nil

public export %inline
HasAttributes IntLit where
  attributes = const Nil

public export %inline
HasAttributes StringLit where
  attributes = const Nil

public export %inline
(HasAttributes a, HasAttributes b) => HasAttributes (a,b) where
  attributes (x,y) = attributes x ++ attributes y

public export %inline
HasAttributes ExtAttribute where
  attributes = pure

public export %inline
HasAttributes a => HasAttributes (Maybe a) where
  attributes = maybe Nil attributes

public export %inline
HasAttributes a => HasAttributes (List a) where
  attributes x = x >>= attributes

public export %inline
HasAttributes a => HasAttributes (List1 a) where
  attributes = attributes . forget

--------------------------------------------------------------------------------
--          Deriving HasAttributes
--------------------------------------------------------------------------------

public export
(all : NP HasAttributes ts) => HasAttributes (NP I ts) where
  attributes = hcconcatMap HasAttributes attributes

public export
(all : NP HasAttributes ts) => HasAttributes (NS I ts) where
  attributes = hcconcatMap HasAttributes attributes

public export
(all : POP HasAttributes ts) => HasAttributes (SOP I ts) where
  attributes = hcconcatMap HasAttributes attributes

--------------------------------------------------------------------------------
--          Claims
--------------------------------------------------------------------------------

||| General type of a `attributes` function with the given list
||| of implicit and auto-implicit arguments, plus the given argument type
||| to be displayed.
export
generalAttrType : (implicits : List Arg) -> (arg : TTImp) -> TTImp
generalAttrType is arg = piAll `(~(arg) -> ExtAttributeList) is

||| Top-level function declaration implementing the `attrbutes` function for
||| the given data type.
export
attrClaim : (fun : Name) -> (p : ParamTypeInfo) -> Decl
attrClaim fun p =
  let arg := p.applied
      tpe := generalAttrType (allImplicits p "HasAttributes") arg
   in public' fun tpe

||| Top-level declaration of the `HasAttributes`
||| implementation for the given data type.
export
attrImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
attrImplClaim impl p = implClaim impl (implType "HasAttributes" p)

--------------------------------------------------------------------------------
--          Definitions
--------------------------------------------------------------------------------

||| Top-level definition of the `Show` implementation for the given data type.
export
attrImplDef : (fun, impl : Name) -> Decl
attrImplDef f i = def i [patClause (var i) (var "MkHasAttributes" `app` var f)]

parameters (nms : List Name)
  ttimp : BoundArg 1 Regular -> TTImp
  ttimp (BA (MkArg _  _ _ t) [x] _) = assertIfRec nms t `(attributes ~(varStr x))

  rsh : SnocList TTImp -> TTImp
  rsh [<] = `(Nil)
  rsh st  = `(listBind ~(listOf st) id)

  export
  attrClauses : (fun : Name) -> TypeInfo -> List Clause
  attrClauses fun ti = map clause ti.cons
    where clause : Con ti.arty ti.args -> Clause
          clause c =
            let ns  := freshNames "x" c.arty
                bc  := bindCon c ns
                lhs := var fun `app` bc
                st  := ttimp <$> boundArgs regular c.args [ns]
             in patClause lhs (rsh st)

  export
  attrDef : Name -> TypeInfo -> Decl
  attrDef fun ti = def fun (attrClauses fun ti)

--------------------------------------------------------------------------------
--          Deriving
--------------------------------------------------------------------------------

namespace Derive

  ||| Generate declarations and implementations for `HasAttributes`
  ||| for a given data type.
  export
  HasAttributes : List Name -> ParamTypeInfo -> Res (List TopLevel)
  HasAttributes nms p =
    let fun  := funName p "attributes"
        impl := implName p "HasAttributes"
     in Right [ TL (attrClaim fun p) (attrDef nms fun p.info)
              , TL (attrImplClaim impl p) (attrImplDef fun impl)
              ]

--------------------------------------------------------------------------------
--          Tests and Proofs
-----------------------------------------------------------------------------

isParenTrue : all Attribute.isParenOrQuote (unpack "(){}[]\"") = True
isParenTrue = Refl

isParenFalse : any Attribute.isParenOrQuote (unpack "=!?><:;,.-_") = False
isParenFalse = Refl

isCommaOrParenTrue : all Attribute.isCommaOrParenOrQuote (unpack ",(){}[]\"") = True
isCommaOrParenTrue = Refl

isCommaOrParenFalse : any Attribute.isCommaOrParenOrQuote (unpack "=!?><:;.-_") = False
isCommaOrParenFalse = Refl
