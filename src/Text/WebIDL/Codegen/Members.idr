module Text.WebIDL.Codegen.Members

import Data.List
import Text.WebIDL.Codegen.Rules
import Text.WebIDL.Codegen.Types
import Text.WebIDL.Codegen.Util
import Text.WebIDL.Types
import Text.WebIDL.Encoder as E

export
Pretty FloatLit where
  pretty v = pretty $ E.floatLit v

export
Pretty IntLit where
  pretty (Hex k) = pretty $ E.toDigits "0x" 16 k
  pretty (Oct 0) = pretty "0"
  pretty (Oct k) = pretty $ E.toDigits "0o" 8 k
  pretty (I x)   = pretty x

export
Pretty ConstValue where
  pretty (B x) = pretty x
  pretty (F x) = pretty x
  pretty (I x) = pretty x

--------------------------------------------------------------------------------
--          Subtyping
--------------------------------------------------------------------------------

export
jsType : Identifier -> Supertypes -> String
jsType n (MkSupertypes parents ms) =
  let mixins = sortedNubOn id ms

      doc = indent {ann = ()} 2 $
              vsep [ ""
                   , "public export"
                   , "JSType" <++> pretty n.value <++> "where"
                   , "  parents = " <++>
                       prettyList (map (pretty . value) parents)
                   , ""
                   , "  mixins = " <++>
                       prettyList (map (pretty . value) mixins)
                   ]
   in show doc

--------------------------------------------------------------------------------
--          Constants
--------------------------------------------------------------------------------

export
constants : List Const -> List String
constants = map (show . const) . sortBy (comparing name)
  where const : Const -> Doc ()
        const (MkConst t n v) =
          indent 2 $ vsep [ ""
                          , "public export"
                          , pretty n.value <++> ":" <++> pretty t
                          , pretty n.value <++> "=" <++> pretty v
                          ]

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

-- argNames : Stream IdrisIdent
-- argNames = map fromString $ "a" :: "b" :: "c" :: "d" :: "e" :: "f" :: "g" ::
--                             "h" :: "i" :: "j" :: "k" :: "l" :: "m" :: "n" :: 
--                             "o" :: "p" :: "q" :: "r" :: "s" :: "t" :: "u" :: 
--                             "v" :: "w" :: "x" :: "y" :: "z" :: 
--                             map (\v => "x" ++ show v) [the Integer 1 ..]

primType : (name : IdrisIdent) -> List CGType -> CGType -> Doc ()
primType name ts t = typeDecl (Prim $ show name) (primReturnType t) $
                      map primType ts
  where primType : CGType -> Doc ()


funType : (name : IdrisIdent) -> List Arg -> CGType -> Doc ()
-- funType n args t =
--   let args2 = map (toPrettyParam . snd) args
-- 
--    in typeDeclWithImplicits n (returnType t) [] args2
-- 
--   where toPrettyParam : ArgumentRest -> Doc ()
--         toPrettyParam (Optional (e,tpe) (MkArgName n) def) =
--           prettyArg (fromString n) (pretty tpe)
-- 
--         toPrettyParam (Mandatory tpe (MkArgName n)) =
--           prettyArg (fromString n) (pretty tpe)
-- 
--         -- TODO: Properly support varargs
--         toPrettyParam (VarArg tpe (MkArgName n)) =
--           prettyArg (fromString n) (pretty tpe)
-- 
funImpl : (name : IdrisIdent) -> List Arg -> Doc ()
funImpl name n = ?foo
--  let pn   = pretty name
--      vals = map pretty $ take n argNames
--      lhs  = hsep (pn :: vals ++ ["="])
--      rhs  = hsep ["primToJSIO" 
--                  , "\"" <+> pn <+> "\""
--                  , "$"
--                  , pretty (Prim $ show name)
--                  , align $ sep $ map (\v => parens ("toJS" <++> v)) vals
--                  ]
--
--   in sep [lhs, indent 2 rhs]

fun : IdrisIdent -> List Arg -> CGType -> Doc ()
fun ii args t = vsep [ "export"
                     , funType ii args t
                     , funImpl ii args
                     ] 
-- 
-- readonly : Identifier -> Attribute -> List (Doc ())
-- readonly i (MkAttribute _ t (MkAttributeName n)) =
--   let ii = fromString n
--    in [ ""
--       , pretty $ attrGetFFI n
--       , primType ii 1 t
--       , ""
--       , fun ii [objArg i] t
--       ]

attributeSetFFI : AttributeName -> Identifier -> CGType -> String
attributeSetFFI n obj t =
   show $ vsep [ ""
               , pretty $ attrSetFFI n
               , primType (setter n) [Ident obj, t] t
               ]

attributeSet : AttributeName -> Identifier -> CGType -> String
attributeSet n obj t =
   show $ vsep [ ""
               , fun (setter n) [objArg obj, valArg t] Undefined
               ]

attributeGetFFI : AttributeName -> Identifier -> CGType -> String
attributeGetFFI n obj t =
   show $ vsep [ ""
               , pretty $ attrGetFFI n
               , primType (fromString n.value) [Ident obj] t
               ]

attributeGet : AttributeName -> Identifier -> CGType -> String
attributeGet n obj t =
   show $ vsep [ ""
               , fun (fromString n.value) [objArg obj] t
               ]

optionalAttributeSetFFI : AttributeName -> Identifier -> CGType -> String
optionalAttributeSetFFI n obj t =
   show $ vsep [ ""
               , pretty $ attrSetFFI n
               , primType (setter n) [Ident obj, t] t
               ]

optionalAttributeSet : AttributeName -> Identifier -> CGType -> String
optionalAttributeSet n obj t =
   show $ vsep [ ""
               , fun (setter n) [objArg obj, valArg t] Undefined
               ]

optionalAttributeGetFFI : AttributeName -> Identifier -> CGType -> String
optionalAttributeGetFFI n obj t =
   show $ vsep [ ""
               , pretty $ attrGetFFI n
               , primType (fromString n.value) [Ident obj] t
               ]

optionalAttributeGet : AttributeName -> Identifier -> CGType -> String
optionalAttributeGet n obj t =
   show $ vsep [ ""
               , fun (fromString n.value) [objArg obj] t
               ]
-- 
-- codegenForReading : Env -> Attribute -> Bool
-- codegenForReading (MkEnv _ _ cbs) = not . isCallback cbs . type
-- 
-- readwrite : Env -> Identifier -> Attribute -> List (Doc ())
-- readwrite e i a =
--   if codegenForReading e a
--      then readonly i a ++ writeonly i a
--      else writeonly i a
-- 
-- -- TODO: Change Identifier to IdrisIdent here
-- export
-- readOnlyAttributes :  Env
--                    -> Identifier
--                    -> List (Readonly Attribute)
--                    -> List String
-- readOnlyAttributes e i = map (show . indent 2 . vsep . readonly i) 
--                        . sortBy (comparing name)
--                        . filter (codegenForReading e)
--                        . map value
-- 
-- -- TODO: Change Identifier to IdrisIdent here
-- export
-- attributes :  Env
--            -> Identifier
--            -> List Attribute
--            -> List String
-- attributes e i = map (show . indent 2 . vsep . readwrite e i) 
--                . sortBy (comparing name)
-- 
-- -- TODO: Change Identifier to IdrisIdent here
-- export
-- readOnlyAttributesPrim :  Env
--                        -> Identifier
--                        -> List (Readonly Attribute)
--                        -> List String
-- readOnlyAttributesPrim = ?roprim
-- 
-- -- TODO: Change Identifier to IdrisIdent here
-- export
-- attributesPrim :  Env
--                -> Identifier
--                -> List Attribute
--                -> List String
-- attributesPrim = ?rwPrim

function : CGFunction -> String

primFunction : CGFunction -> String
primFunction (AttributeSet n o t) = attributeSet n o t
primFunction (AttributeGet n o t) = attributeGet n o t
primFunction (OptionalAttributeSet n o t) = ?primFunction_rhs_3
primFunction (OptionalAttributeGet n o t d) = ?primFunction_rhs_4
primFunction (Constructor name args optionalArgs) = ?primFunction_rhs_5
primFunction (Regular name args optionalArgs returnType) = ?primFunction_rhs_6
primFunction (VarArg name args varArg returnType) = ?primFunction_rhs_7

export
functions : List CGFunction -> List String
functions = map function . sortBy (comparing priority)

export
primFunctions : List CGFunction -> List String
primFunctions = map primFunction . sortedNubOn priority
