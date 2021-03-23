module Text.WebIDL.Parser

import Data.SOP
import Data.List.Elem
import Data.List1
import Text.Lexer
import Text.Parser
import Text.WebIDL.Types
import Text.WebIDL.Lexer

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

public export
IdlGrammarAny : (b : Bool) -> Type -> Type
IdlGrammarAny b t = Grammar (TokenData IdlToken) b t

public export
IdlGrammar : Type -> Type
IdlGrammar = IdlGrammarAny True

public export
IdlGrammar' : Type -> Type
IdlGrammar' = IdlGrammarAny False

tok : String -> (IdlToken -> Maybe a) -> IdlGrammar a
tok s f = terminal s (f . tok)

withKey : String -> (String -> Maybe a) -> IdlGrammar a
withKey s f = tok s \case (Key $ MkKeyword s _) => f s
                          _                     => Nothing

intLit : IdlGrammar IntLit
intLit = tok "Int Lit" \case ILit n => Just n
                             _      => Nothing

stringLit : IdlGrammar StringLit
stringLit = tok "String Lit" \case SLit s => Just s
                                   _      => Nothing

floatLit : IdlGrammar FloatLit
floatLit = tok "Float Lit" \case FLit v => Just v
                                 _      => Nothing

--------------------------------------------------------------------------------
--          Symbols
--------------------------------------------------------------------------------

symbol : Char -> IdlGrammar ()
symbol c = tok ("Symbol " ++ show c) \case Other (Symb v) => guard (c == v)
                                           _              => Nothing

comma : IdlGrammar ()
comma = symbol ','

ellipsis : IdlGrammar ()
ellipsis = tok "Ellipsis" \case Other Ellipsis => Just ()
                                _              => Nothing

inParens : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inParens g = symbol '(' *> g <* symbol ')'

inBrackets : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inBrackets g = symbol '[' *> g <* symbol ']'

inBraces : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inBraces g = symbol '{' *> g <* symbol '}'

inAngles : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inAngles g = symbol '<' *> g <* symbol '>'

inAnyParens : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inAnyParens g = inParens g <|> inBrackets g <|> inBraces g

sepList1 : Char -> IdlGrammar a -> IdlGrammar (List1 a)
sepList1 c g =   [| (g <* symbol c) ::: sepBy (symbol c) g |]
             <|> map (\x => x ::: Nil) g

--------------------------------------------------------------------------------
--          Identifiers
--------------------------------------------------------------------------------

export
key : String -> IdlGrammar ()
key s = tok s \case Key (MkKeyword i _) => guard (i == s)
                    _                   => Nothing

export
ident : IdlGrammar Identifier
ident = tok "identifier" \case Ident i => Just i
                               _       => Nothing

export
keyword : IdlGrammar Keyword
keyword = tok "keyword" \case Key i => Just i
                              _     => Nothing

||| IdentifierList :: identifier Identifiers
||| Identifiers :: , identifier Identifiers ε
export
identifierList : IdlGrammar IdentifierList
identifierList = [| ident ::: many (comma *> ident) |]

--------------------------------------------------------------------------------
--          Extended Attributes
--------------------------------------------------------------------------------

symbolUnless : String -> (Char -> Bool) -> IdlGrammar Symbol
symbolUnless s f = tok s \case Other s => fromSym s
                               _       => Nothing
  where fromSym : Symbol -> Maybe Symbol
        fromSym Ellipsis = Just Ellipsis
        fromSym (Symb c) = if f c then Nothing else Just (Symb c)
        

otherSym : IdlGrammar Symbol -> IdlGrammar Other
otherSym sym = choice {t = List} [ map (\v => inject v) intLit
                                 , map (\v => inject v) stringLit
                                 , map (\v => inject v) floatLit
                                 , map (\v => inject v) ident
                                 , map (\v => inject v) sym
                                 , map (\v => inject v) keyword
                                 ]

export
other : IdlGrammar Other
other = otherSym $ symbolUnless "other" isCommaOrParenOrQuote

export
otherOrComma : IdlGrammar Other
otherOrComma = otherSym $ symbolUnless "otherOrComma" isParenOrQuote

export
eaInner : IdlGrammar' EAInner
eaInner =   [| EAIParens (inAnyParens eaInner) eaInner |]
        <|> [| EAIOther otherOrComma eaInner |]
        <|> pure EAIEmpty

export
extAttribute : IdlGrammar ExtAttribute
extAttribute =   [| EAParens (inAnyParens eaInner) (optional extAttribute) |]
             <|> [| EAOther other (optional extAttribute) |]

export
extAttrs1 : IdlGrammar ExtAttributeList
extAttrs1 = forget <$> inBrackets (sepBy1 comma extAttribute)

export
extAttributes : IdlGrammar' ExtAttributeList
extAttributes = extAttrs1 <|> pure Nil

export
attributed : IdlGrammar a -> IdlGrammar (Attributed a)
attributed g = [| (,) extAttrs1 g |] <|> map (Nil,) g

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

bufferRelated : IdlGrammar BufferRelatedType
bufferRelated = withKey "BufferRelated"
                  \case "ArrayBuffer"       => Just ArrayBuffer
                        "DataView"          => Just DataView
                        "Int8Array"         => Just Int8Array
                        "Int16Array"        => Just Int16Array
                        "Int32Array"        => Just Int32Array
                        "Uint8Array"        => Just Uint8Array
                        "Uint16Array"       => Just Uint16Array
                        "Uint32Array"       => Just Uint32Array
                        "Uint8ClampedArray" => Just Uint8ClampedArray
                        "Float32Array"      => Just Float32Array
                        "Float64Array"      => Just Float64Array
                        _                   => Nothing

stringType : IdlGrammar StringType
stringType = withKey "stringType"
               \case "ByteString" => Just ByteString
                     "DOMString"  => Just DOMString
                     "USVString"  => Just USVString
                     _            => Nothing

export
primitive : IdlGrammar PrimitiveType
primitive =   key "unsigned"     *> map Unsigned int
          <|> key "unrestricted" *> map Unrestricted float
          <|> map Signed int
          <|> map Restricted float
          <|> withKey "Primitive" \case "boolean"   => Just Boolean 
                                        "byte"      => Just Byte
                                        "octet"     => Just Octet
                                        "bigint"    => Just BigInt
                                        "undefined" => Just Undefined
                                        _           => Nothing

  where int : IdlGrammar IntType
        int =   (key "long"  *> key "long" $> LongLong)
            <|> (key "long"  $> Long)
            <|> (key "short" $> Short)

        float : IdlGrammar FloatType
        float = withKey "FloatType" \case "double" => Just Dbl
                                          "float"  => Just Float
                                          _        => Nothing

constType : IdlGrammar ConstType
constType = map CP primitive <|> map CI ident

nullable : IdlGrammar a -> IdlGrammar (Nullable a)
nullable g = map MaybeNull (g <* symbol '?') <|> map NotNull g


mutual
  -- Type ::
  --     SingleType
  --     UnionType Null
  -- 
  -- SingleType ::
  --     DistinguishableType
  --     any
  --     PromiseType
  -- PromiseType ::
  --     Promise < Type >
  export
  idlType : IdlGrammar IdlType
  idlType =   (key "any" $> Any)
          <|> map Promise (key "Promise" *> inAngles idlType)
          <|> map D distinguishableType
          <|> map U (nullable union)

  -- TypeWithExtendedAttributes ::
  --     ExtendedAttributeList Type
  attrTpe : IdlGrammar (Attributed IdlType)
  attrTpe = attributed idlType

  -- RecordType ::
  --     record < StringType , TypeWithExtendedAttributes >
  recrd : IdlGrammar Distinguishable
  recrd = Record <$> (key "record" *> symbol '<' *> stringType)
                 <*> (comma *> attrTpe <* symbol '>')

  -- DistinguishableType ::
  --     PrimitiveType Null
  --     StringType Null
  --     identifier Null
  --     sequence < TypeWithExtendedAttributes > Null
  --     object Null
  --     symbol Null
  --     BufferRelatedType Null
  --     FrozenArray < TypeWithExtendedAttributes > Null
  --     ObservableArray < TypeWithExtendedAttributes > Null
  --     RecordType Null
  distinguishable : IdlGrammar Distinguishable
  distinguishable =
        map P primitive
    <|> map S stringType
    <|> map B bufferRelated
    <|> (key "object" $> Object)
    <|> (key "symbol" $> Symbol)
    <|> map Sequence (key "sequence" *> inAngles attrTpe)
    <|> map FrozenArray (key "FrozenArray" *> inAngles attrTpe)
    <|> map ObservableArray (key "ObservableArray" *> inAngles attrTpe)
    <|> recrd
    <|> map I ident

  distinguishableType : IdlGrammar (Nullable Distinguishable)
  distinguishableType = nullable distinguishable

  -- UnionType ::
  --     ( UnionMemberType or UnionMemberType UnionMemberTypes )
  --
  -- UnionMemberTypes ::
  --     or UnionMemberType UnionMemberTypes
  --     ε
  union : IdlGrammar UnionType
  union = inParens $ do (a :: b :: t) <- sepBy (key "or") unionMember
                          | _ => fail "Non enough Union members"
                        pure (UT a b t)

  -- UnionMemberType ::
  --     ExtendedAttributeList DistinguishableType
  --     UnionType Null
  unionMember : IdlGrammar UnionMemberType
  unionMember =   map UD (attributed distinguishableType)
              <|> map UU (nullable union)

optionalType : IdlGrammar' OptionalType
optionalType = optional (symbol ',' *> attributed idlType)

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

boolLit : IdlGrammar Bool
boolLit = (key "false" $> False) <|> (key "true" $> True)

constValue : IdlGrammar ConstValue
constValue = map B boolLit <|> map F floatLit <|> map I intLit

defaultV : IdlGrammar' Default
defaultV =   (symbol '=' *> (
                   (symbol '[' *> symbol ']' $> EmptyList)
               <|> (symbol '{' *> symbol '}' $> EmptySet)
               <|> (key "null" $> Null)
               <|> map S stringLit
               <|> map C constValue
             )) <|> pure None

argName : IdlGrammar ArgumentName
argName =   withKey "ArgumentNameKeyword"
              (map (MkArgName . value) . ArgumentNameKeyword.refine)
        <|> map (MkArgName . value) ident

export
argumentRest : IdlGrammar ArgumentRest
argumentRest =   [| Optional (key "optional" *> attrTpe) argName defaultV |]
             <|> [| VarArg    (idlType <* ellipsis) argName |]
             <|> [| Mandatory idlType argName |]

argumentList : IdlGrammar' ArgumentList
argumentList = sepBy comma (attributed argumentRest)

optArgList : IdlGrammar' ArgumentList
optArgList = inParens argumentList <|> pure Nil

--------------------------------------------------------------------------------
--          Member
--------------------------------------------------------------------------------

def : List String -> IdlGrammar a -> IdlGrammar a
def [] g = g <* symbol ';'
def (h :: t) g = key h *> def t g

export
const : IdlGrammar Const
const = def ["const"] [| MkConst constType ident (symbol '=' *> constValue) |]

special : IdlGrammar Special
special =   (key "getter"  $> Getter)
        <|> (key "setter"  $> Setter)
        <|> (key "deleter" $> Deleter)

opName : IdlGrammar OperationName
opName =   (key "includes" $> MkOpName "includes")
       <|> map (\(MkIdent s) => MkOpName s) ident

regularOperation : IdlGrammar RegularOperation
regularOperation =
  def [] [| MkOp (pure ()) idlType (optional opName) (inParens argumentList) |]

specialOperation : IdlGrammar SpecialOperation
specialOperation =
  def [] [| MkOp special idlType (optional opName) (inParens argumentList) |]

export
operation : IdlGrammar Operation
operation = map specToOp specialOperation <|> map regToOp regularOperation

callbackInterfaceMember : IdlGrammar CallbackInterfaceMember
callbackInterfaceMember =   map (\v => inject v) const
                        <|> map (\v => inject v) regularOperation

dictMember : IdlGrammar DictionaryMemberRest
dictMember =   def ["required"] [| Required extAttributes idlType ident |]
           <|> def [] [| Optional idlType ident defaultV |]

inheritance : IdlGrammar' Inheritance
inheritance = optional (symbol ':' *> ident)

attributeName : IdlGrammar AttributeName
attributeName =  withKey "AttributeNameKeyword"
                   (map (MkAttributeName . value) . AttributeNameKeyword.refine)
             <|> map (MkAttributeName . value) ident

readonly : IdlGrammar a -> IdlGrammar (Readonly a)
readonly g = key "readonly" *> map MkRO g

inherit : IdlGrammar a -> IdlGrammar (Inherit a)
inherit g = key "inherit" *> map MkI g

attribute : IdlGrammar Attribute
attribute = def ["attribute"]
            [| MkAttribute extAttributes idlType attributeName |]

stringifier : IdlGrammar Stringifier
stringifier =   key "stringifier" *> (
                map (\v => inject v) attribute
            <|> map (\v => inject v) (readonly attribute)
            <|> map (\v => inject v) regularOperation
            <|> map (\v => inject v) (symbol ';')
            )

static : IdlGrammar StaticMember
static =   key "static" *> (
           map (\v => inject v) attribute
       <|> map (\v => inject v) (readonly attribute)
       <|> map (\v => inject v) regularOperation
       )

maplike : IdlGrammar Maplike
maplike = def ["maplike"] $ inAngles [| MkMaplike (attributed idlType)
                                        (symbol ',' *> attributed idlType) |]

setlike : IdlGrammar Setlike
setlike = def ["setlike"] $ inAngles [| MkSetlike (attributed idlType) |]

namespaceMember : IdlGrammar NamespaceMember
namespaceMember =   map (\v => inject v) regularOperation
                <|> map (\v => inject v) (readonly attribute)

constructor_ : IdlGrammar Constructor
constructor_ = def ["constructor"] (map MkConstructor $ inParens argumentList)

partialInterfaceMember : IdlGrammar PartialInterfaceMember
partialInterfaceMember =
      map IConst const
  <|> map IOp operation
  <|> map IAttr attribute
  <|> map IAttrRO (readonly attribute)
  <|> map IAttrInh (inherit attribute)
  <|> map IMap maplike
  <|> map IMapRO (readonly maplike)
  <|> map ISet setlike
  <|> map ISetRO (readonly setlike)
  <|> map IStr stringifier
  <|> map IStatic static
  <|> def ["iterable"] (inAngles [| IIterable (attributed idlType)
                                            optionalType |])
  <|> def ["async","iterable"] (
        do p  <- inAngles [| (,) (attributed idlType) optionalType |]
           as <- optArgList
           pure (IAsync (fst p) (snd p) as))

mixinMember : IdlGrammar MixinMember
mixinMember =   map MConst const
            <|> map MOp regularOperation
            <|> map MAttr attribute
            <|> map MAttrRO (readonly attribute)
            <|> map MStr stringifier

export
interfaceMember : IdlGrammar InterfaceMember
interfaceMember =   map (\v => inject v) constructor_
                <|> map (\v => inject v) partialInterfaceMember

members : IdlGrammar a -> IdlGrammar (List $ Attributed a)
members g = inBraces (many $ attributed g)

--------------------------------------------------------------------------------
--          Definition
--------------------------------------------------------------------------------

-- optional trailing comma
enumLits : IdlGrammar (List1 StringLit)
enumLits = sepList1 ',' stringLit <* (symbol ',' <|> pure ())

callback : IdlGrammar Callback
callback = def ["callback"] [| MkCallback ident (symbol '=' *> idlType)
                                          (inParens argumentList) |]

callbackInterface : IdlGrammar CallbackInterface
callbackInterface =
  def ["callback","interface"] [| MkCallbackInterface ident
                                    (members callbackInterfaceMember) |]

dictionary : IdlGrammar Dictionary
dictionary = def ["dictionary"]
             [| MkDictionary ident inheritance (members dictMember) |]

enum : IdlGrammar Enum
enum = def ["enum"] [| MkEnum ident (inBraces enumLits) |]

iface : IdlGrammar Interface
iface = def ["interface"]
        [| MkInterface ident inheritance (members interfaceMember) |]

includes : IdlGrammar Includes
includes = def [] [| MkIncludes ident (key "includes" *> ident) |]

mixin : IdlGrammar Mixin
mixin = def ["interface","mixin"] [| MkMixin ident (members mixinMember) |]

nspace : IdlGrammar Namespace
nspace = def ["namespace"] [| MkNamespace ident (members namespaceMember) |]

pdictionary : IdlGrammar PDictionary
pdictionary = def ["partial","dictionary"]
              [| MkPDictionary ident (members dictMember) |]

pnamespace : IdlGrammar PNamespace
pnamespace = def ["partial","namespace"]
             [| MkPNamespace ident (members namespaceMember) |]

pmixin : IdlGrammar PMixin
pmixin = def ["partial","interface","mixin"] 
         [| MkPMixin ident (members mixinMember) |]

pinterface : IdlGrammar PInterface
pinterface = def ["partial","interface"]
             [| MkPInterface ident (members partialInterfaceMember) |]

typedef : IdlGrammar Typedef
typedef = def ["typedef"] [| MkTypedef extAttributes idlType ident |]

export
definition : IdlGrammar Definition
definition =
      map (\v => inject v) callbackInterface
  <|> map (\v => inject v) callback
  <|> map (\v => inject v) dictionary
  <|> map (\v => inject v) enum
  <|> map (\v => inject v) iface
  <|> map (\v => inject v) includes
  <|> map (\v => inject v) mixin
  <|> map (\v => inject v) nspace
  <|> map (\v => inject v) pdictionary
  <|> map (\v => inject v) pinterface
  <|> map (\v => inject v) pmixin
  <|> map (\v => inject v) pnamespace
  <|> map (\v => inject v) typedef


export
definitions : IdlGrammar Definitions
definitions = concatMap toDefinitions <$> some (attributed definition)

--------------------------------------------------------------------------------
--          Parsing WebIDL
--------------------------------------------------------------------------------

toParseErr : ParseError (TokenData IdlToken) -> Err
toParseErr (Error x []) = ParseErr x
toParseErr (Error x (MkToken l c t :: _)) = ParseErrAt x l c t

export
parseIdl : IdlGrammar a -> String -> Either Err a
parseIdl g s = do ts <- mapFst LexErr (lexIdlNoNoise s)
                  (res,Nil) <- mapFst toParseErr (parse g ts)
                    | (_,MkToken l c t :: _) => Left (NoEOI l c t)
                  pure res
