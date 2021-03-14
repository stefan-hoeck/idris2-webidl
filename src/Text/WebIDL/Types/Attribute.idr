module Text.WebIDL.Types.Attribute

import Data.SOP
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Symbol

import Generics.Derive

%language ElabReflection

public export
isParen : Char -> Bool
isParen '(' = True
isParen ')' = True
isParen '[' = True
isParen ']' = True
isParen '{' = True
isParen '}' = True
isParen _   = False

public export
isCommaOrParen : Char -> Bool
isCommaOrParen ',' = True
isCommaOrParen c   = isParen c

public export
0 Other : Type
Other = NS I [Integer,FloatLit,StringLit,Identifier,Symbol]

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

%runElab derive "EAInner" [Generic,Meta,Eq,Show]

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

%runElab derive "ExtAttribute" [Generic,Meta,Eq,Show]


||| ExtendedAttributeList ::
|||   [ ExtendedAttribute ExtendedAttributes ]
|||   ε
||| 
||| ExtendedAttributes ::
|||   , ExtendedAttribute ExtendedAttributes
|||   ε
public export
0 ExtAttributeList : Type
ExtAttributeList = List ExtAttribute


--------------------------------------------------------------------------------
--          Tests and Proofs
--------------------------------------------------------------------------------

isParenTrue : all Attribute.isParen (unpack "(){}[]") = True
isParenTrue = Refl

isParenFalse : any Attribute.isParen (unpack "=!?><:;,.-_") = False
isParenFalse = Refl

isCommaOrParenTrue : all Attribute.isCommaOrParen (unpack ",(){}[]") = True
isCommaOrParenTrue = Refl

isCommaOrParenFalse : any Attribute.isCommaOrParen (unpack "=!?><:;.-_") = False
isCommaOrParenFalse = Refl
