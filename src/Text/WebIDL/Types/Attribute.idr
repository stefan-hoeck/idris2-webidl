module Text.WebIDL.Types.Attribute

import Data.SOP
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Symbol

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
|||   ( ExtendedAttributeInner ) ExtendedAttributeInner
|||   [ ExtendedAttributeInner ] ExtendedAttributeInner
|||   { ExtendedAttributeInner } ExtendedAttributeInner
|||   OtherOrComma ExtendedAttributeInner
|||   ε
public export
0 ExtendedAttributeList : Type
ExtendedAttributeList = List Other

-- [TODO] ExtendedAttributeList ::
--     [ ExtendedAttribute ExtendedAttributes ]
--     ε
-- 
-- [TODO] ExtendedAttributes ::
--     , ExtendedAttribute ExtendedAttributes
--     ε
-- 
-- [TODO] ExtendedAttribute ::
--     ( ExtendedAttributeInner ) ExtendedAttributeRest
--     [ ExtendedAttributeInner ] ExtendedAttributeRest
--     { ExtendedAttributeInner } ExtendedAttributeRest
--     Other ExtendedAttributeRest
-- 
-- [TODO] ExtendedAttributeRest ::
--     ExtendedAttribute
--     ε
-- 

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
