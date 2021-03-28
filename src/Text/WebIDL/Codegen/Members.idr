module Text.WebIDL.Codegen.Members

import Data.List
import Text.WebIDL.Codegen.Inheritance
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
jsType : Settings -> Identifier -> String
jsType (MkSettings ts mi _)  n =
  let MkSupertypes parents ms = supertypes ts mi n

      mixins = sortedNubOn id ms

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

argNames : Stream IdrisIdent
argNames = map fromString $ "a" :: "b" :: "c" :: "d" :: "e" :: "f" :: "g" ::
                            "h" :: "i" :: "j" :: "k" :: "l" :: "m" :: "n" :: 
                            "o" :: "p" :: "q" :: "r" :: "s" :: "t" :: "u" :: 
                            "v" :: "w" :: "x" :: "y" :: "z" :: 
                            map (\v => "x" ++ show v) [the Integer 1 ..]

primType : (name : IdrisIdent) -> Nat -> IdlType -> Doc ()
primType name n x = typeDecl (Prim $ show name) (primReturnType x) $
                      replicate n "AnyPtr"

funType : (name : IdrisIdent) -> ArgumentList -> IdlType -> Doc ()
funType n args t =
  let args2 = map (toPrettyParam . snd) args

   in typeDeclWithImplicits n (returnType t) [] args2

  where toPrettyParam : ArgumentRest -> Doc ()
        toPrettyParam (Optional (e,tpe) (MkArgName n) def) =
          prettyArg (fromString n) (pretty tpe)

        toPrettyParam (Mandatory tpe (MkArgName n)) =
          prettyArg (fromString n) (pretty tpe)

        -- TODO: Properly support varargs
        toPrettyParam (VarArg tpe (MkArgName n)) =
          prettyArg (fromString n) (pretty tpe)

funImpl : (name : IdrisIdent) -> Nat -> Doc ()
funImpl name n =
  let pn   = pretty name
      vals = map pretty $ take n argNames
      lhs  = hsep (pn :: vals ++ ["="])
      rhs  = hsep ["primToJSIO" 
                  , "\"" <+> pn <+> "\""
                  , "$"
                  , pretty (Prim $ show name)
                  , align $ sep $ map (\v => parens ("toJS" <++> v)) vals
                  ]

   in sep [lhs, indent 2 rhs]

fun : IdrisIdent -> ArgumentList -> IdlType -> Doc ()
fun ii args t = vsep [ "export"
                     , funType ii args t
                     , funImpl ii (length args)
                     ] 

readonly : Identifier -> Attribute -> List (Doc ())
readonly i (MkAttribute _ t (MkAttributeName n)) =
  let ii = fromString n
   in [ ""
      , pretty $ attrGetFFI n
      , primType ii 1 t
      , ""
      , fun ii [objArg i] t
      ]

writeonly : Identifier -> Attribute -> List (Doc ())
writeonly i a@(MkAttribute _ t (MkAttributeName n)) =
  [ ""
  , pretty $ attrSetFFI n
  , primType (setter n) 2 t
  , ""
  , fun (setter n) [objArg i, valArg t] undefined
  ]

codegenForReading : Settings -> Attribute -> Bool
codegenForReading (MkSettings _ _ cbs) = not . isCallback cbs . type

readwrite : Settings -> Identifier -> Attribute -> List (Doc ())
readwrite ss i a =
  if codegenForReading ss a
     then readonly i a ++ writeonly i a
     else writeonly i a

-- TODO: Change Identifier to IdrisIdent here
export
readOnlyAttributes :  Settings
                   -> Identifier
                   -> List (Readonly Attribute)
                   -> List String
readOnlyAttributes ss i = map (show . indent 2 . vsep . readonly i) 
                        . sortBy (comparing name)
                        . filter (codegenForReading ss)
                        . map value

-- TODO: Change Identifier to IdrisIdent here
export
attributes :  Settings
           -> Identifier
           -> List Attribute
           -> List String
attributes ss i = map (show . indent 2 . vsep . readwrite ss i) 
                . sortBy (comparing name)
