module Text.WebIDL.Types.Attribute

import Data.SOP
import Text.WebIDL.Types.Numbers
import Text.WebIDL.Types.StringLit
import Text.WebIDL.Types.Identifier
import Text.WebIDL.Types.Symbol

public export
0 Other : Type
Other = NS I [Integer,FloatLit,StringLit,Identifier,Symbol]

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
-- [TODO] ExtendedAttributeInner ::
--     ( ExtendedAttributeInner ) ExtendedAttributeInner
--     [ ExtendedAttributeInner ] ExtendedAttributeInner
--     { ExtendedAttributeInner } ExtendedAttributeInner
--     OtherOrComma ExtendedAttributeInner
--     ε
