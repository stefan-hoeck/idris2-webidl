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
--          Functions
--------------------------------------------------------------------------------

obj : Kind -> ArgType
obj = Regular . identToType

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

primType : (name : IdrisIdent) -> List ArgType -> ReturnType -> Doc ()
primType name ts t = typeDecl name (primReturnType t) (map pretty ts)


-- funType : (name : IdrisIdent) -> List Arg -> ArgType -> Doc ()
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
-- funImpl : (name : IdrisIdent) -> List Arg -> Doc ()
-- funImpl name n = ?foo
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

-- fun : IdrisIdent -> List Arg -> ArgType -> Doc ()
-- fun ii args t = vsep [ "export"
--                      , funType ii args t
--                      , funImpl ii args
--                      ] 

--------------------------------------------------------------------------------
--          Attributes
--------------------------------------------------------------------------------

attributeSetFFI : AttributeName -> Kind -> ArgType -> String
attributeSetFFI n o t =
   show $ vsep [ ""
               , "export"
               , pretty $ attrSetFFI n
               , primType (primSetter o n) [obj o, t] Undefined
               ]

attributeGetFFI : AttributeName -> Kind -> ReturnType -> String
attributeGetFFI n o t =
   show $ vsep [ ""
               , "export"
               , pretty $ attrGetFFI n
               , primType (primGetter o n) [obj o] t
               ]

opFFI : Kind -> OperationName -> List ArgType -> ReturnType -> String
opFFI k n as t =
  let args = Regular (identToType k) :: as
   in show $ vsep [ ""
                  , pretty $ funFFI n (length args)
                  , primType (primOp k n) args t
                  ]

constructorFFI : Kind -> List ArgType -> String
constructorFFI n args =
  show $ vsep [ ""
              , pretty $ conFFI n (length args)
              , primType (primConstructor n) args (FromIdl $ identToType n)
              ]

-- attributeSet : AttributeName -> Identifier -> ArgType -> String
-- attributeSet n obj t =
--    show $ vsep [ ""
--                , fun (setter n) [objArg obj, valArg t] Undefined
--                ]
-- 
-- attributeGet : AttributeName -> Identifier -> ArgType -> String
-- attributeGet n obj t =
--    show $ vsep [ ""
--                , fun (fromString n.value) [objArg obj] t
--                ]

function : CGFunction -> Maybe String
function _ = Nothing

toArgTypeList : Args -> List ArgType
toArgTypeList (VarArg as v)    = map (Regular . type) as ++ [VarArg v.type]
toArgTypeList (NoVarArg as os) = map (Regular . type) as ++
                                 map (OptionalArg . type) os

prim : CGFunction -> Maybe String
prim (AttributeSet n o t)           = Just $ attributeSetFFI n o (Regular t)
prim (AttributeGet n o t)           = Just $ attributeGetFFI n o (FromIdl t)
prim (OptionalAttributeSet n o t)   = Just $ attributeSetFFI n o (OptionalArg t)
prim (OptionalAttributeGet n o t d) = Just $ attributeGetFFI n o (Optional t)
prim (Constructor n args)           = Just $ constructorFFI n (toArgTypeList args)
prim (Regular k n args t)           = Just $ opFFI k n (toArgTypeList args) t

export
functions : List CGFunction -> List String
functions = mapMaybe function . sortBy (comparing priority)

export
primFunctions : List CGFunction -> List String
primFunctions = mapMaybe prim . sortedNubOn priority
